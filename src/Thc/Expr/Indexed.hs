{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
  , E.Lit(..)
  , fromLiteral

  -- * Patterns
  , E.Pattern(..)

  -- * Named terms
  , NamedTerm

  -- * Errors
  , EvalError(..)
  , TypeError(..)

  -- * Functions exported for testing
  , reduce
  , evalForPat
  ) where

import Control.Arrow
import Control.Exception.Safe
import Control.Monad.State.Lazy
import Control.Monad.Trans.Except
import Data.Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Lazy as Map

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
  | Record [(String, Term)]
  | Ann Term T.Type
  | Tagged String Term
  | Case (NonEmpty.NonEmpty (E.Pattern, Term))
  deriving (Eq, Show)

type NamedTerm = E.Term

instance E.Lit Term where
  bool = Lit . E.Bool
  int = Lit . E.Int
  unit = Lit E.Unit

-- |
-- @fromNamed t@ converts a 'NamedTerm' to a 'Term'.
--
-- >>> l = E.int 0
-- >>> fromNamed l
-- Right (Lit (Int 0))
-- >>> fromNamed (E.Abs (E.PVar "a") (T.Int T.:->: T.Bool) $ E.App (E.Var "a") l)
-- Right (Abs (PVar "a") (Int :->: Bool) (App (Var "a" 0 1) (Lit (Int 0))))
--
-- Unbound variables cause the result 'Nothing'.
--
-- >>> fromNamed (E.Var "x")
-- Left (Unbound "x")
--
-- In 'E.Abs', conflicts between the parameter's pattern and its corresponding
-- type cause the result 'Nothing' (i.e. @λ(nn,):Int.nn@ where @(/x/,)@ is
-- a 1-tuple whose only element is /x/).
--
-- >>> fromNamed (E.Abs (E.PTuple [E.PVar "nn"]) T.Int $ E.Var "nn")
-- Left (PatternMismatch (PTuple [PVar "nn"]) Int)
fromNamed :: NamedTerm -> Either EvalError Term
fromNamed = fromNamed' emptyContext

fromNamed' :: MonadError m EvalError => Context -> NamedTerm -> m Term

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
fromNamed' ctx (E.Record ts) = Record <$> mapM (runKleisli . second . Kleisli $ fromNamed' ctx) ts
fromNamed' ctx (E.Ann t ty) = flip Ann ty <$> fromNamed' ctx t
fromNamed' ctx (E.Tagged i t) = Tagged i <$> fromNamed' ctx t

-- |
-- Binds variables to a @Context@ verifying the type of a pattern.
--
-- >>> bindPattern (E.PVar "a") T.Bool emptyContext :: Maybe Context
-- Just [("a",Bool)]
bindPattern :: MonadError m EvalError => E.Pattern -> T.Type -> Context -> m Context
bindPattern (E.PVar i) ty ctx = return $ addName i ty ctx
bindPattern p @ (E.PTuple ps) ty @ (T.Tuple ts) ctx
  | not $ null ds          = errorE $ DuplicateVariables p
  | length ps == length ts = return $ addNames (zip ps ts) ctx
  | otherwise              = errorE $ PatternMismatch p ty
  where
    ds :: [String]
    ds = dups ps
bindPattern p @ (E.PTuple ps) ty _ = errorE $ PatternMismatch p ty -- Note that type variables are currently not supported.

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
addNames xs ctx = foldl addNameFromPattern ctx xs

addNameFromPattern :: Context -> (E.Pattern, T.Type) -> Context
addNameFromPattern ctx (E.PVar i, t) = (i, t) : ctx
addNameFromPattern ctx (E.PTuple ps, T.Tuple ts) = addNames (zip ps ts) ctx

addName :: String -> T.Type -> Context -> Context
addName i ty ctx = (i, ty) : ctx

name2index :: MonadError m EvalError => String -> Context -> m Int
name2index i [] = errorE $ Unbound i
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
    walk c (Ann t ty) = Ann (walk c t) ty
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
    walk c (Ann t ty) = Ann (walk c t) ty
    walk c l @ (Lit _) = l

-- subst
substTop :: (Term, Term) -> Term
substTop = subst 0 . shift 1 *** id >>> app >>> shift (-1)

-- |
-- Evaluates a 'Term' to its normal form. Well-typed terms cannot diverge.
--
-- >>> eval (E.int 3)
-- Lit (Int 3)
-- >>> eval (Abs (E.PVar "x") T.Int (Var "x" 0 1))
-- Abs (PVar "x") Int (Var "x" 0 1)
-- >>> eval (App (Abs (E.PVar "x") T.Int (Var "x" 0 1)) (E.bool False))
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
-- >>> eval (App x (E.int 2))
-- App (Lit (Int 2)) (Lit (Int 2))
--
-- @eval (App x x)@ diverges.
eval :: Term -> Term
eval t = maybe t eval $ eval1 t

eval1 :: Term -> Maybe Term
eval1 (App (Abs p _ t2) t1) = reduce p t1 t2
eval1 (App t1 t2) = flip App t2 <$> eval1 t1
eval1 (Tuple ts) = Tuple <$> f ts
  where
    f :: [Term] -> Maybe [Term]
    f [] = Nothing
    f (t : ts) = case eval1 t of
      Just t' -> return $ t' : ts
      Nothing -> (t :) <$> f ts
eval1 (Ann t ty) = return $ case eval1 t of
  Just t' -> Ann t' ty
  Nothing -> t
eval1 _ = Nothing

-- |
-- @reduce p t1 t2@ performs beta-reduction.
--
-- >>> reduce (E.PVar "x") (E.int 2) (E.int 1)
-- Lit (Int 1)
-- >>> reduce (E.PVar "x") (E.int 2) (Var "x" 0 1)
-- Lit (Int 2)
--
-- >>> p = E.tuplePat ["x", "y"]
-- >>> tuple = Tuple [E.int 2, E.int 3]
-- >>> reduce p tuple (Var "y" 0 2)
-- Lit (Int 3)
-- >>> reduce p tuple (Var "x" 1 2)
-- Lit (Int 2)
--
-- >>> p = E.PTuple [E.tuplePat ["x", "y"], E.tuplePat ["z", "a"]]
-- >>> tuple = Tuple [Tuple [E.int 2, E.int 3], Tuple [E.int 4, E.int 5]]
-- >>> reduce p tuple (Var "z" 1 4)
-- Lit (Int 4)
-- >>> tuple = Tuple [Tuple [E.int 2, Abs (E.PVar "z") T.Int $ Var "z" 0 1], Tuple [E.int 4, E.int 5]]
-- >>> reduce p tuple (Var "y" 2 4)
-- Abs (PVar "z") Int (Var "z" 0 1)
reduce :: MonadThrow m => E.Pattern -> Term -> Term -> m Term
reduce p t1 t2 = fmap (shift (-l)) . flip evalStateT 0 $ reduce' p t1 t2
  where
    l = length $ E.bounds p

    reduce' :: MonadThrow m => E.Pattern -> Term -> Term -> StateT Int m Term
    reduce' (E.PVar _) t1 t2 = state $ \n -> (subst n (shift l t1) t2, n + 1)
    reduce' (E.PTuple ps) (Tuple ts) t = foldrM (uncurry reduce') t $ zip ps ts
    reduce' p t1 t2 = do
      t1' <- evalForPat p t1
      reduce' p t1' t2

evalForPat :: MonadThrow m => E.Pattern -> Term -> m Term
evalForPat (E.PVar _) t = return t
evalForPat (E.PTuple ps) (Tuple ts) = Tuple <$> uncurry evalForPat `mapM` zip ps ts
evalForPat p @ (E.PTuple _) t =
  case eval1 t of
    Just t' -> evalForPat p t'
    Nothing -> throwPatTerm p t

throwPatTerm :: MonadThrow m => E.Pattern -> Term -> m a
throwPatTerm p t = do
  a <- typeOf t
  throw $ case a of
    Right ty -> WrongPattern p ty
    Left e -> IllTyped t e

data EvalException
  = WrongPattern E.Pattern T.Type
  | IllTyped Term TypeError
  | WrongIndex Context Int
  deriving Show

instance Exception EvalException

fromLiteral :: Term -> Maybe E.Literal
fromLiteral (Lit l) = return l
fromLiteral _ = Nothing

data TypeError
  = EvalError EvalError
  | VariantError String Term (Map.Map String T.Type)
  -- |
  -- @IllTypedApp t ty@ means the type of @t@ should have been @ty -> _@ where
  -- @_@ is a placeholder.
  | IllTypedApp Term T.Type
  | BareVariant String Term
  -- | @TypeMismatch s t@ indicates got type @s@ does not match expected type @t@.
  | TypeMismatch T.Type T.Type
  | IncompatibleArms (NonEmpty.NonEmpty (E.Pattern, Term))
  deriving (Eq, Show)

typeOf :: MonadThrow m => Term -> m (Either TypeError T.Type)
typeOf = runExceptT . typeOf' emptyContext

typeOf' :: MonadThrow m => Context -> Term -> ExceptT TypeError m T.Type
typeOf' ctx (Var _ x _) = getTypeFromContext ctx x
typeOf' ctx (Abs p ty1 t) = do
  ctx' <- ExceptT . return . left EvalError $ bindPattern p ty1 ctx
  ty2 <- typeOf' ctx' t
  return $ ty1 T.:->: ty2
typeOf' ctx (App t1 t2) = do
  ty1 <- typeOf' ctx t1
  ty2 <- typeOf' ctx t2
  case ty1 of
    u1 T.:->: u2 | u1 == ty2 -> return u2
    _                        -> throwE $ IllTypedApp t1 ty2
typeOf' ctx (Lit l) = return $ E.typeOfLiteral l
typeOf' ctx (Tuple ts) = T.Tuple <$> mapM (typeOf' ctx) ts
typeOf' ctx (Record ts) = T.Record <$> mapM (runKleisli . second . Kleisli $ typeOf' ctx) ts
typeOf' ctx (Tagged i t) = throwE $ BareVariant i t
typeOf' ctx (Ann (Tagged i t) ty @ (T.Variant ts)) = do
  ty' <- ExceptT . return . maybe (Left $ VariantError i t ts) return $ Map.lookup i ts
  typeOf' ctx $ Ann t ty'
  return ty
typeOf' ctx (Ann t ty) = do
  ty' <- typeOf' ctx t
  if ty == ty'
    then return ty
    else throwE $ TypeMismatch ty' ty
typeOf' ctx (Case ts) = do
  x NonEmpty.:| xs <- mapM typeWithPat ts
  if and $ map (== x) xs
    then return x
    else throwE $ IncompatibleArms ts

getTypeFromContext :: MonadThrow m => Context -> Int -> m T.Type
getTypeFromContext ctx n
  | length ctx <= n = throw $ WrongIndex ctx n
  | otherwise       = return . snd $ ctx !! n

typeWithPat :: MonadThrow m => (E.Pattern, Term) -> ExceptT TypeError m T.Type
typeWithPat = undefined

class Monad m => MonadError m e where
  errorE :: e -> m a

instance MonadError Maybe e where
  errorE e = Nothing

instance MonadError (Either e) e where
  errorE = Left

data EvalError
  = Unbound String
  | DuplicateVariables E.Pattern
  | PatternMismatch E.Pattern T.Type
  deriving (Eq, Show)
