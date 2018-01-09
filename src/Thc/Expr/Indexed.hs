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
  , BindError(..)

  -- * Exceptions
  , MonadThrowPlus(..)

  -- * Functions exported for testing
  , reduce
  , evalForPat
  , shift
  ) where

import Control.Applicative
import Control.Arrow
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Data.Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Lazy as Map
import Data.Monoid

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
  | Case Term (NonEmpty.NonEmpty (E.Pattern, Term))
  | Fold T.Type Term
  | Unfold T.Type Term
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
-- Right (Abs (PVar "a") (Int -> Bool) (App (Var "a" 0 1) (Lit (Int 0))))
--
-- Unbound variables cause it to report the error.
--
-- >>> fromNamed (E.Var "x")
-- Left (Unbound "x")
fromNamed :: NamedTerm -> Either EvalError Term
fromNamed = fromNamed' emptyContext

fromNamed' :: ContextU -> NamedTerm -> Either EvalError Term

fromNamed' ctx (E.Var i) = do
  x <- name2index i ctx
  return $ Var i x $ length ctx

fromNamed' ctx (E.Abs p ty t) = do
  ctx' <- left BindError $ bindPatternU ctx p
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
fromNamed' ctx (E.Case t as) = Case <$> fromNamed' ctx t <*> mapM f as
  where
    f (p, t) = do
      ctx' <- left BindError $ bindPatternU ctx p
      t' <- fromNamed' ctx' t
      return (p, t')

bindPatternU :: ContextU -> E.Pattern -> Either BindError ContextU
bindPatternU ctx p = bindPattern1 Binder
  { onTuple = \ps _ -> return . zip ps $ repeat ()
  , onVariant = \ctx _ p _ -> bindPatternU ctx p
  } ctx p ()

-- |
-- Binds variables to a @Context@ verifying the type of a pattern.
--
-- >>> bindPattern emptyContext (E.PVar "a") T.Bool
-- Right [("a",Bool)]
bindPattern :: Context -> E.Pattern -> T.Type -> Either BindError Context
bindPattern = bindPattern1 Binder
  { onTuple = f
  , onVariant = g
  }
    where
      f ps ty @ (T.Tuple ts)
        | length ps == length ts = return $ zip ps ts
        | otherwise              = Left $ PatternMismatch (E.PTuple ps) ty
      f ps ty = Left $ PatternMismatch (E.PTuple ps) ty -- Note that type variables are currently not supported.

      g ctx i p tv @ (T.Variant ts) = do
        ty <- maybe (Left $ PatternMismatch (E.PVariant i p) tv) return $ Map.lookup i ts
        bindPattern ctx p ty
      g _ i p ty = Left $ PatternMismatch (E.PVariant i p) ty

data BindError
  = PatternMismatch E.Pattern T.Type
  | DuplicateVariables E.Pattern
  deriving (Eq, Show)

data Binder a = Binder
  { onTuple   :: [E.Pattern] -> a -> Either BindError [(E.Pattern, a)]
  , onVariant :: Context1 a -> String -> E.Pattern -> a -> Either BindError (Context1 a)
  }

bindPattern1 :: Binder a -> Context1 a -> E.Pattern -> a -> Either BindError (Context1 a)
bindPattern1 _ ctx (E.PVar i) x = return $ addName ctx i x
bindPattern1 binder ctx p @ (E.PTuple ps) x
  | not $ null ds = Left $ DuplicateVariables p
  | otherwise     = onTuple binder ps x >>= foldM f ctx
    where
      ds :: [String]
      ds = dups ps

      f ctx (p, x) = bindPattern1 binder ctx p x
bindPattern1 binder ctx (E.PVariant i p) x = onVariant binder ctx i p x
bindPattern1 _ ctx (E.PLiteral _) _ = return ctx

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
    g (E.PVariant i p) acc = g p acc
    g (E.PLiteral _) _ = return ()

    both f = f *** f

type Context1 a = [(String, a)]

type Context = Context1 T.Type

type ContextU = Context1 ()

emptyContext :: Context1 a
emptyContext = []

addName :: Context1 a -> String -> a -> Context1 a
addName ctx i x = (i, x) : ctx

name2index :: String -> Context1 a -> Either EvalError Int
name2index i [] = Left $ Unbound i
name2index i (x : xs)
  | i == fst x = return 0
  | otherwise  = (1 +) <$> name2index i xs

data EvalError
  = Unbound String
  | BindError BindError
  deriving (Eq, Show)

shift :: Int -> Term -> Term
shift d = tmap f 0
  where
    f c i x n
      | x >= c    = Var i (x + d) (n + d) -- free
      | otherwise = Var i x $ n + d       -- bound

subst :: Int -> Term -> Term -> Term
subst j s = tmap f 0
  where
    f c i x n
      | x == j + c = shift c s
      | otherwise  = Var i x n

tmap :: (Int -> String -> Int -> Int -> Term) -> Int -> Term -> Term
tmap f = walk
  where
    walk :: Int -> Term -> Term
    walk c (Var i x n)   = f c i x n
    walk c (Abs p ty t') = Abs p ty $ walk (h c p) t'
    walk c (App t1 t2)   = App (walk c t1) (walk c t2)
    walk c (Tuple ts)    = Tuple $ walk c `map` ts
    walk c (Ann t ty)    = Ann (walk c t) ty
    walk c (Tagged i t)  = Tagged i $ walk c t
    walk c (Case t as)   = Case (walk c t) $ NonEmpty.map (g c) as
    walk c (Fold ty t)   = Fold ty $ walk c t
    walk c (Unfold ty t) = Unfold ty $ walk c t
    walk c l @ (Lit _)   = l

    g c = fst &&& app . first (walk . h c)
    h c p = c + E.nbounds p

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
eval :: MonadThrowPlus m => Term -> m Term
eval t = eval1 t >>= maybe (return t) eval

eval1 :: MonadThrowPlus m => Term -> m (Maybe Term)
eval1 (App (Abs p _ t2) t1) = return <$> reduce p t1 t2
eval1 (App t1 t2) = fmap (flip App t2) <$> eval1 t1
eval1 (Tuple ts) = fmap Tuple <$> f ts
  where
    f :: MonadThrowPlus m => [Term] -> m (Maybe [Term])
    f [] = return Nothing
    f (t : ts) = eval1 t >>= maybe (qq t) (g ts)

    qq :: MonadThrowPlus m => Term -> m (Maybe [Term])
    qq t = fmap (t :) <$> f ts

    g :: MonadThrow m => [Term] -> Term -> m (Maybe [Term])
    g ts = return . return . (: ts)
eval1 (Ann t ty) = return . maybe t (flip Ann ty) <$> eval1 t
eval1 (Case t as) = eval1 t >>= maybe result next
  where
    next   = return . Just . flip Case as
    result = fmap return . getAlt $ foldMap (Alt . f) as
    f      = uncurry $ flip reduce t
eval1 (Fold ty t) = fmap (Fold ty) <$> eval1 t
eval1 (Unfold ty1 (Fold ty2 t)) = return . maybe t (Unfold ty1 . Fold ty2) <$> eval1 t
eval1 (Unfold ty t) = fmap (Unfold ty) <$> eval1 t
eval1 _ = return Nothing

class (MonadThrow m, MonadPlus m) => MonadThrowPlus m where
  mthrowM :: Exception e => e -> m a

instance MonadThrowPlus [] where
  mthrowM _ = []

instance MonadThrowPlus Maybe where
  mthrowM _ = Nothing

instance MonadThrowPlus m => MonadThrowPlus (StateT s m) where
  mthrowM e = lift $ throwM e

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
reduce :: MonadThrowPlus m => E.Pattern -> Term -> Term -> m Term
reduce p t1 t2 = fmap (shift (-l)) . flip evalStateT 0 $ reduce' p t1 t2
  where
    l = E.nbounds p

    reduce' :: MonadThrowPlus m => E.Pattern -> Term -> Term -> StateT Int m Term
    reduce' (E.PVar _) t1 t2 = state $ \n -> (subst n (shift l t1) t2, n + 1)
    reduce' (E.PTuple ps) (Tuple ts) t = foldrM (uncurry reduce') t $ zip ps ts
    reduce' pv @ (E.PVariant i1 p) tt @ (Tagged i2 t1) t2
      | i1 == i2  = reduce' p t1 t2
      -- TODO: Can't determine the type of 'tt' at this time, so the exception
      -- can be difficult to understand.
      | otherwise = throwPatTerm pv tt
    reduce' p t1 t2 = do
      t1' <- evalForPat p t1
      reduce' p t1' t2

evalForPat :: MonadThrowPlus m => E.Pattern -> Term -> m Term
evalForPat (E.PVar _) t = return t
evalForPat (E.PTuple ps) (Tuple ts) = Tuple <$> uncurry evalForPat `mapM` zip ps ts
evalForPat p @ (E.PTuple _) t = do
  t' <- eval1 t
  case t' of
    Just t'' -> evalForPat p t''
    Nothing -> throwPatTerm p t
evalForPat p0 @ (E.PVariant i1 p) t0 @ (Tagged i2 t)
  | i1 == i2  = Tagged i1 <$> evalForPat p t
  | otherwise = throwPatTerm p0 t0 -- TODO: The same problem as 'reduce' have.
evalForPat p @ (E.PVariant i _) t = eval1 t >>= maybe (throwPatTerm p t) (evalForPat p)
evalForPat (E.PLiteral _) t = eval t -- Literals are values and all values are normal form.

throwPatTerm :: MonadThrowPlus m => E.Pattern -> Term -> m a
throwPatTerm p t = do
  a <- typeOf t
  mthrowM $ case a of
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
  = BindTypeError BindError
  | VariantError String Term (Map.Map String T.Type)
  -- |
  -- @IllTypedApp t ty@ means the type of @t@ should have been @ty -> _@ where
  -- @_@ is a placeholder.
  | IllTypedApp Term T.Type
  | BareVariant String Term
  -- | @TypeMismatch s t@ indicates got type @s@ does not match expected type @t@.
  | TypeMismatch T.Type T.Type
  | IncompatibleArms (NonEmpty.NonEmpty (E.Pattern, Term))
  | FoldError T.Type
  deriving (Eq, Show)

typeOf :: MonadThrow m => Term -> m (Either TypeError T.Type)
typeOf = runExceptT . typeOf' emptyContext

typeOf' :: MonadThrow m => Context -> Term -> ExceptT TypeError m T.Type
typeOf' ctx (Var _ x _) = getTypeFromContext ctx x
typeOf' ctx (Abs p ty1 t) = (ty1 T.:->:) <$> typeWithPat ctx ty1 (p, t)
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
typeOf' ctx (Case t ts) = do
  ty <- typeOf' ctx t
  x NonEmpty.:| xs <- forM ts $ typeWithPat ctx ty
  if and $ map (== x) xs
    then return x
    else throwE $ IncompatibleArms ts
typeOf' ctx (Fold ty t) = typeOfFold ctx t ty
typeOf' ctx (Unfold ty t) = typeOfUnfold ctx t ty

typeOfFold :: MonadThrow m => Context -> Term -> T.Type -> ExceptT TypeError m T.Type
typeOfFold ctx t = f
  where
    f u @ (T.Rec _ ty0) = do
      ty <- typeOf' ctx t
      let ty' = T.substTop (u, ty0)
      if ty == ty'
        then return u
        else throwE $ TypeMismatch ty ty'
    f ty = throwE $ FoldError ty

typeOfUnfold :: MonadThrow m => Context -> Term -> T.Type -> ExceptT TypeError m T.Type
typeOfUnfold ctx t = f
  where
    f ty' @ (T.Rec _ ty0) = do
      ty <- typeOf' ctx t
      if ty == ty'
        then return $ T.substTop (ty, ty0)
        else throwE $ TypeMismatch ty ty'
    f ty = throwE $ FoldError ty

getTypeFromContext :: MonadThrow m => Context -> Int -> m T.Type
getTypeFromContext ctx n
  | length ctx <= n = throw $ WrongIndex ctx n
  | otherwise       = return . snd $ ctx !! n

typeWithPat :: MonadThrow m => Context -> T.Type -> (E.Pattern, Term) -> ExceptT TypeError m T.Type
typeWithPat ctx ty (p, t) = do
  ctx' <- ExceptT . return . left BindTypeError $ bindPattern ctx p ty
  typeOf' ctx' t
