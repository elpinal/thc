module Thc.Expr.Indexed
  (
  -- $setup

  -- * Terms
    Term(..)
  , fromNamed
  , principal
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
  , PatternMatchError(..)

  -- * Exceptions
  , MonadThrowPlus(..)

  -- * Functions exported for testing
  , reduce
  , evalForPat
  , shift
  ) where

import Control.Arrow
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
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
  , onLiteral = \ctx _ _ -> return ctx
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
  , onLiteral = h
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

      h ctx l ty
        | E.typeOfLiteral l == ty = return ctx
        | otherwise               = Left $ PatternMismatch (E.PLiteral l) ty

data BindError
  = PatternMismatch E.Pattern T.Type
  | DuplicateVariables E.Pattern
  deriving (Eq, Show)

data Binder a = Binder
  { onTuple   :: [E.Pattern] -> a -> Either BindError [(E.Pattern, a)]
  , onVariant :: Context1 a -> String -> E.Pattern -> a -> Either BindError (Context1 a)
  , onLiteral :: Context1 a -> E.Literal -> a -> Either BindError (Context1 a)
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
bindPattern1 binder ctx (E.PLiteral l) x = onLiteral binder ctx l x

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
-- >>> eval x == Just x
-- True
-- >>> eval (App x (E.int 2))
-- App (Lit (Int 2)) (Lit (Int 2))
--
-- @eval (App x x)@ diverges.
eval :: MonadThrowPlus m => Term -> m Term
eval t = eval1 t >>= maybe (return t) eval

eval1 :: MonadThrowPlus m => Term -> m (Maybe Term)
eval1 (App (Abs p _ t2) t1)     = either2maybe <$> reduce p t1 t2
eval1 (App t1 t2)               = flip App t2 <$$> eval1 t1
eval1 (Tuple ts)                = Tuple <$$> evalTuple1 ts
eval1 (Ann t ty)                = maybe t (flip Ann ty) <-$> eval1 t
eval1 (Case t as)               = evalCase1 t as
eval1 (Fold ty t)               = Fold ty <$$> eval1 t
eval1 (Unfold ty1 (Fold ty2 t)) = maybe t (Unfold ty1 . Fold ty2) <-$> eval1 t
eval1 (Unfold ty t)             = Unfold ty <$$> eval1 t
eval1 _                         = return Nothing

-- | @evalTuple1 ts@ applies 'eval1' to the first term of @ts@ that is not the
-- normal form.
evalTuple1 :: MonadThrowPlus m => [Term] -> m (Maybe [Term])
evalTuple1 [] = return Nothing
evalTuple1 (t : ts) = eval1 t >>= maybe ((t :) <$$> evalTuple1 ts) (liftJust . (: ts))

evalCase1 :: MonadThrowPlus m => Term -> NonEmpty.NonEmpty (E.Pattern, Term) -> m (Maybe Term)
evalCase1 t as = eval1 t >>= maybe result next
  where
    next   = liftJust . flip Case as
    result = fmap either2maybe . getAlt $ foldMap (Alt . f) as
    f      = uncurry $ flip reduce t

either2maybe :: Either e a -> Maybe a
either2maybe (Right x) = return x
either2maybe (Left x) = Nothing

liftJust :: MonadThrow m => a -> m (Maybe a)
liftJust = return . Just

(<$$>) :: (Functor f, Functor f') => (a -> b) -> f (f' a) -> f (f' b)
(<$$>) = fmap . fmap

(<-$>) :: (Functor f, Monad m) => (a -> b) -> f a -> f (m b)
(<-$>) f = fmap $ return . f

class (MonadThrow m, MonadPlus m) => MonadThrowPlus m where
  mthrowM :: Exception e => e -> m a

instance MonadThrowPlus [] where
  mthrowM _ = []

instance MonadThrowPlus Maybe where
  mthrowM _ = Nothing

instance MonadThrowPlus IO where
  mthrowM = throwM

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
reduce :: MonadThrowPlus m => E.Pattern -> Term -> Term -> m (Either PatternMatchError Term)
reduce p t1 t2 = flip evalStateT 0 . runExceptT $ shift (-l) <$> reduce' p t1 t2
  where
    l = E.nbounds p

    reduce' :: MonadThrowPlus m => E.Pattern -> Term -> Term -> ExceptT PatternMatchError (StateT Int m) Term
    reduce' (E.PVar _) t1 t2 = lift . state $ \n -> (subst n (shift l t1) t2, n + 1)
    reduce' (E.PTuple ps) (Tuple ts) t = foldrM (uncurry reduce') t $ zip ps ts
    reduce' pv @ (E.PVariant i1 p) tt @ (Tagged i2 t1) t2
      | i1 == i2  = reduce' p t1 t2
      -- TODO: Can't determine the type of 'tt' at this time, so the exception
      -- can be difficult to understand.
      | otherwise = lift . lift $ throwPatTerm pv tt
    reduce' (E.PLiteral l1) (Lit l2) t
      | l1 == l2  = return t
      | otherwise = throwE $ LiteralMismatch l1 l2
    reduce' p t1 t2 = do
      t1' <- lift $ evalForPat p t1
      reduce' p t1' t2

data PatternMatchError
  = LiteralMismatch E.Literal E.Literal
  deriving (Eq, Show)

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
  a <- principal t
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
  | TError T.Error -- TODO: better name
  deriving (Eq, Show)

getTypeFromContext :: MonadThrow m => Context -> Int -> m T.Type
getTypeFromContext ctx n
  | length ctx <= n = throw $ WrongIndex ctx n
  | otherwise       = return . snd $ ctx !! n

bindPatternE :: Monad m => Context -> E.Pattern -> T.Type -> ExceptT TypeError m Context
bindPatternE ctx p ty = ExceptT . return . left BindTypeError $ bindPattern ctx p ty

principal :: MonadThrow m => Term -> m (Either TypeError T.Type)
principal = principal' emptyContext

principal' :: MonadThrow m => Context -> Term -> m (Either TypeError T.Type)
principal' ctx t = do
  (ty, s) <- reconstruct ctx t
  return $ T.apply s <$> ty

type Reconstructor m a = ExceptT TypeError (StateT T.Subst (StateT Int m)) a

reconstruct :: MonadThrow m => Context -> Term -> m (Either TypeError T.Type, T.Subst)
reconstruct ctx = flip evalStateT 0 . flip runStateT T.emptySubst . runExceptT . recon ctx

recon :: MonadThrow m => Context -> Term -> Reconstructor m T.Type
recon ctx (Var _ x _)   = lift $ getTypeFromContext ctx x
recon ctx (Abs p ty t)  = reconAbs ctx p ty t
recon ctx (App t1 t2)   = reconApp ctx t1 t2
recon ctx (Lit l)       = return $ E.typeOfLiteral l
recon ctx (Tuple ts)    = T.Tuple <$> mapM (recon ctx) ts
recon ctx (Record ts)   = reconRecord ctx ts
recon ctx (Ann (Tagged i t) ty) = reconTagged ctx i t ty
recon ctx (Ann t ty)    = reconAnn ctx t ty
recon ctx (Tagged i t)  = throwE $ BareVariant i t -- TODO:
recon ctx (Case t ts)   = reconCase ctx t ts
recon ctx (Fold ty t)   = reconFold ctx t ty
recon ctx (Unfold ty t) = reconUnfold ctx t ty

reconAbs :: MonadThrow m => Context -> E.Pattern -> T.Type -> Term -> Reconstructor m T.Type
reconAbs ctx p ty t = (ty T.:->:) <$> reconWithPat ctx ty (p, t)

freshVar :: Monad m => Reconstructor m T.Type
freshVar = T.Id <$> (lift . lift) T.freshVar

reconApp :: MonadThrow m => Context -> Term -> Term -> Reconstructor m T.Type
reconApp ctx t1 t2 = do
  v <- freshVar
  ty1 <- recon ctx t1
  ty2 <- recon ctx t2
  unify $ T.fromList [(ty1, ty2 T.:->: v)]
  return v

reconRecord :: MonadThrow m => Context -> [(String, Term)] -> Reconstructor m T.Type
reconRecord ctx ts = T.Record <$> mapM (secondM $ recon ctx) ts

secondM :: Monad m => (a -> m b) -> (c, a) -> m (c, b)
secondM = runKleisli . second . Kleisli

reconAnn :: MonadThrow m => Context -> Term -> T.Type -> Reconstructor m T.Type
reconAnn ctx t ty2 = do
  ty1 <- recon ctx t
  unify $ T.fromList [(ty1, ty2)]
  return ty1

reconCase :: MonadThrow m => Context -> Term -> NonEmpty.NonEmpty (E.Pattern, Term) -> Reconstructor m T.Type
reconCase ctx t ts = do
  v <- freshVar
  ty <- recon ctx t
  xs <- forM ts $ reconWithPat ctx ty
  unify . T.fromList . NonEmpty.toList $ ((,) v) <$> xs
  return v

reconWithPat :: MonadThrow m => Context -> T.Type -> (E.Pattern, Term) -> Reconstructor m T.Type
reconWithPat ctx ty (p, t) = do
  ctx' <- bindPatternE ctx p ty
  recon ctx' t

reconFold :: MonadThrow m => Context -> Term -> T.Type -> Reconstructor m T.Type
reconFold ctx t tyU @ (T.Rec _ ty1) = do
  ty2 <- recon ctx t
  unify $ T.fromList [(ty2, T.substTop (tyU, ty1))]
  return tyU
reconFold _ _ ty = throwE $ FoldError ty -- TODO: syntactically disallow.

reconUnfold :: MonadThrow m => Context -> Term -> T.Type -> Reconstructor m T.Type
reconUnfold ctx t tyU @ (T.Rec _ ty1) = do
  ty2 <- recon ctx t
  unify $ T.fromList [(ty2, tyU)]
  return $ T.substTop (ty2, ty1)
reconUnfold _ _ ty = throwE $ FoldError ty -- TODO: syntactically disallow

reconTagged :: MonadThrow m => Context -> String -> Term -> T.Type -> Reconstructor m T.Type
reconTagged ctx i t ty @ (T.Variant ts) = do
  ty' <- ExceptT . return . maybe (Left $ VariantError i t ts) return $ Map.lookup i ts
  recon ctx $ Ann t ty'
  return ty
reconTagged ctx i t ty = error "variant error" -- FIXME

unify :: Monad m => T.Constraints -> Reconstructor m ()
unify = mapM_ f
  where
    f (t1, t2) = do
      g <- T.apply <$> lift get
      s <- ExceptT . return . left TError $ g t1 `T.mgu` g t2
      lift $ modify (s T.@@)
