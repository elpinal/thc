{-# LANGUAGE FlexibleInstances #-}

module Thc.Expr.Indexed
  (
  -- $setup

  -- * Terms
    Term(..)
  , fromNamed
  , typeOf
  , eval

  -- * Literals
  , E.Literal(..)
  , fromLiteral

  -- * Patterns
  , E.Pattern(..)

  -- * Named terms
  , NamedTerm

  -- * Functions exported for testing
  , evalTuple
  , evalForPat
  ) where

import Control.Arrow
import Control.Exception.Safe
import Control.Monad.State.Lazy

import qualified Thc.Expr as E
import qualified Thc.Type as T

-- $setup
-- >>> import qualified Thc.Expr as E
-- >>> import qualified Thc.Type as T

data Term =
    -- |
    -- Name hint, de Bruijn index, and the surrounding context's length for debug
    Var String Int Int
  | Abs E.Pattern T.Type Term
  | App Term Term
  | Lit E.Literal
  | Tuple [Term]
  deriving (Eq, Show)

type NamedTerm = E.Term

-- |
-- @fromNamed t@ converts a 'NamedTerm' to a 'Term'.
--
-- >>> l = E.Lit $ E.Int 0
-- >>> fromNamed l
-- Just (Lit (Int 0))
-- >>> fromNamed (E.Abs (E.PVar "a") (T.Int T.:->: T.Bool) $ E.App (E.Var "a") l)
-- Just (Abs (PVar "a") (Int :->: Bool) (App (Var "a" 0 1) (Lit (Int 0))))
--
-- Unbound variables cause the result 'Nothing'.
--
-- >>> fromNamed (E.Var "x")
-- Nothing
--
-- In 'E.Abs', conflicts between the parameter's pattern and its corresponding
-- type cause the result 'Nothing' (i.e. @λ(nn,):Int.nn@ where @(/x/,)@ is
-- a 1-tuple whose only element is /x/).
--
-- >>> fromNamed (E.Abs (E.PTuple [E.PVar "nn"]) T.Int $ E.Var "nn")
-- Nothing
fromNamed :: NamedTerm -> Maybe Term
fromNamed = fromNamed' emptyContext

fromNamed' :: Context -> NamedTerm -> Maybe Term

fromNamed' ctx (E.Var i) = do
  x <- name2index i ctx
  return $ Var i x $ length ctx

fromNamed' ctx (E.Abs p ty t) = do
  ctx' <- bindPattern p ty ctx
  Abs p ty <$> fromNamed' ctx' t

fromNamed' ctx (E.App t1 t2) = do
  u1 <- fromNamed' ctx t1
  u2 <- fromNamed' ctx t2
  return $ App u1 u2

fromNamed' ctx (E.Lit l) = return $ Lit l
fromNamed' ctx (E.Tuple ts) = Tuple <$> mapM (fromNamed' ctx) ts

-- |
-- Binds variables to a @Context@ verifying the type of a pattern.
--
-- >>> bindPattern (E.PVar "a") T.Bool emptyContext
-- Just [("a",Bool)]
bindPattern :: E.Pattern -> T.Type -> Context -> Maybe Context
bindPattern (E.PVar i) ty ctx = return $ addName i ty ctx
bindPattern (E.PTuple ps) (T.Tuple ts) ctx
  | not $ null ds          = Nothing
  | length ps == length ts = return $ addNames (zip ps ts) ctx
  | otherwise              = Nothing
  where
    ds :: [String]
    ds = dups ps
bindPattern (E.PTuple ps) ty _ = Nothing -- Note that type variables are currently not supported.

-- | @dups xs@ finds duplications in @xs@.
dups :: [E.Pattern] -> [String]
dups = snd . flip execState ([], []) . mapM_ f
  where
    f p = get >>= g p

    g :: E.Pattern -> ([String], [String]) -> State ([String], [String]) ()
    g (E.PVar x) acc =
      case elem x `both` acc of
        (True, True)  -> return ()
        (True, False) -> modify $ second (x :)
        (False, _)    -> modify $ first (x :)
    g (E.PTuple ps) acc = mapM_ (flip g acc) ps

    both f = f *** f

type Context = [(String, T.Type)]

emptyContext :: Context
emptyContext = []

addNames :: [(E.Pattern, T.Type)] -> Context -> Context
addNames xs ctx = foldr addNameFromPattern ctx xs

addNameFromPattern :: (E.Pattern, T.Type) -> Context -> Context
addNameFromPattern ((E.PVar i), t) ctx = (i, t) : ctx
addNameFromPattern ((E.PTuple ps), (T.Tuple ts)) ctx = addNames (zip ps ts) ctx

addName :: String -> T.Type -> Context -> Context
addName i ty ctx = (i, ty) : ctx

name2index :: String -> Context -> Maybe Int
name2index i [] = Nothing
name2index i (x : xs)
  | i == fst x = return 0
  | otherwise  = (1 +) <$> name2index i xs

shift :: Int -> Term -> Term
shift d = walk 0
  where
    walk :: Int -> Term -> Term
    walk c (Var i x n)
      | x >= c    = Var i (x + d) (n + d) -- free
      | otherwise = Var i x $ n + d       -- bound
    walk c (Abs i ty t') = Abs i ty $ walk (c + 1) t'
    walk c (App t1 t2) = App (walk c t1) (walk c t2)
    walk c (Tuple ts) = Tuple $ walk c `map` ts
    walk c l @ (Lit _) = l

subst :: Int -> Term -> Term -> Term
subst j s = walk 0
  where
    walk :: Int -> Term -> Term
    walk c t' @ (Var i x n)
      | x == j + c = shift c s
      | otherwise  = t'
    walk c (Abs i ty t') = Abs i ty $ walk (c + 1) t'
    walk c (App t1 t2) = App (walk c t1) (walk c t2)
    walk c (Tuple ts) = Tuple $ walk c `map` ts
    walk c l @ (Lit _) = l

-- subst
substTop :: (Term, Term) -> Term
substTop = subst 0 . shift 1 *** id >>> app >>> shift (-1)

-- |
-- Evaluates a 'Term' to its normal form. Well-typed terms cannot diverge.
--
-- >>> eval (Lit (E.Int 3))
-- Lit (Int 3)
-- >>> eval (Abs (E.PVar "x") T.Int (Var "x" 0 1))
-- Abs (PVar "x") Int (Var "x" 0 1)
-- >>> eval (App (Abs (E.PVar "x") T.Int (Var "x" 0 1)) (Lit (E.Bool False)))
-- Lit (Bool False)
--
-- Even when given invalid variables:
--
-- >>> eval (Var "x" 100 100)
-- Var "x" 100 100
--
-- For ill-typed terms:
--
-- >>> x = (Abs (E.PVar "x") T.Int $ App (Var "x" 0 1) (Var "x" 0 1))
-- >>> eval x == x
-- True
-- >>> eval (App x (Lit (E.Int 2)))
-- App (Lit (Int 2)) (Lit (Int 2))
--
-- @eval (App x x)@ diverges.
eval :: Term -> Term
eval t = maybe t eval $ eval1 t

eval1 :: Term -> Maybe Term
eval1 (App (Abs (E.PVar _) _ t1) t2) = return $ substTop (t2, t1)
eval1 (App (Abs (E.PTuple _) _ t) (Tuple ts)) = evalTuple t ts
eval1 (App a @ (Abs (E.PTuple _) _ t1) t2) = App a <$> eval1 t2
eval1 (App t1 t2) = flip App t2 <$> eval1 t1
eval1 (Tuple ts) = Tuple <$> f ts
  where
    f :: [Term] -> Maybe [Term]
    f [] = Nothing
    f (t : ts) = case eval1 t of
      Just t' -> return $ t' : ts
      Nothing -> (t :) <$> f ts
eval1 _ = Nothing

evalTuple :: Term -> [Term] -> Maybe Term
evalTuple t ts =
  return . shift (- length ts) . foldl f t $ zip [0..] ts
  where
    f t1 (n, t2) = subst n (shift (n + 1) t2) t1

evalForPat :: MonadThrow m => E.Pattern -> Term -> m Term
evalForPat (E.PVar _) t = return t
evalForPat (E.PTuple ps) (Tuple ts) = fmap Tuple . mapM (uncurry evalForPat) $ zip ps ts
evalForPat p @ (E.PTuple _) t =
  case eval1 t of
    Just t' -> evalForPat p t'
    Nothing -> throwPatTerm p t

throwPatTerm :: MonadThrow m => E.Pattern -> Term -> m a
throwPatTerm p t = throw $
  case typeOf t of
    Just ty -> WrongPattern p ty
    Nothing -> IllTyped t

data EvalException
  = WrongPattern E.Pattern T.Type
  | IllTyped Term
  deriving Show

instance Exception EvalException

fromLiteral :: Term -> Maybe E.Literal
fromLiteral (Lit l) = return l
fromLiteral _ = Nothing

typeOf :: Term -> Maybe T.Type
typeOf = typeOf' emptyContext

typeOf' :: Context -> Term -> Maybe T.Type
typeOf' ctx (Var _ x _) = return $ getTypeFromContext ctx x
typeOf' ctx (Abs p ty1 t) = do
  ctx' <- bindPattern p ty1 ctx
  ty2 <- typeOf' ctx' t
  return $ ty1 T.:->: ty2
typeOf' ctx (App t1 t2) = do
  ty1 <- typeOf' ctx t1
  ty2 <- typeOf' ctx t2
  case ty1 of
    u1 T.:->: u2 | u1 == ty2 -> return u2
    _                        -> Nothing
typeOf' ctx (Lit l) = return $ E.typeOfLiteral l
typeOf' ctx (Tuple ts) = T.Tuple <$> mapM (typeOf' ctx) ts

getTypeFromContext :: Context -> Int -> T.Type
getTypeFromContext ctx n = snd $ ctx !! n

class Monad m => EvalError m where
  errorE :: String -> m a

instance EvalError Maybe where
  errorE e = Nothing

instance EvalError (Either String) where
  errorE = Left
