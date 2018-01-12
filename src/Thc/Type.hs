module Thc.Type
  ( Type(..)
  , TypeId(IdString)
  , idString
  , variant
  , substTop
  , Subst
  , emptySubst
  , (|->)
  , (@@)
  , merge
  , Constraints
  , fromList
  , unify
  , mgu
  , varBind
  , Error(..)
  ) where

import Control.Arrow
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Data.Map.Lazy as Map
import Data.Map.Merge.Lazy hiding (merge)
import qualified Data.Set as Set

data Type =
    Bool
  | Int
  | Unit
  | Type :->: Type
  | Id TypeId -- TODO: naming
  | Var String Int Int
  | Rec String Type
  | Tuple [Type]
  | Record [(String, Type)]
  | Variant (Map.Map String Type)
  deriving (Eq, Ord)

infixr 9 :->:

data TypeId
  = IdString String
  | Fresh Int
  deriving (Eq, Ord)

freshVar :: State Int TypeId
freshVar = state $ \n -> (Fresh n, n + 1)

idString :: String -> Type
idString = Id . IdString

-- | @variant xs@ creates a new 'Variant' from @xs@.
variant :: [(String, Type)] -> Type
variant = Variant . Map.fromList

shift :: Int -> Type -> Type
shift d = tymap f 0
  where
    f c i x n
      | x >= c    = Var i (x + d) (n + d)
      | otherwise = Var i x $ n + d

subst :: Int -> Type -> Type -> Type
subst j s = tymap f 0
  where
    f c i x n
      | x == j + c = shift c s
      | otherwise  = Var i x n

tymap :: (Int -> String -> Int -> Int -> Type) -> Int -> Type -> Type
tymap f = walk
  where
    walk :: Int -> Type -> Type
    walk c Bool         = Bool
    walk c Int          = Int
    walk c Unit         = Unit
    walk c (Id i)       = Id i
    walk c (t1 :->: t2) = walk c t1 :->: walk c t2
    walk c (Var i x n)  = f c i x n
    walk c (Rec i t)    = Rec i $ walk (c + 1) t
    walk c (Tuple ts)   = Tuple $ map (walk c) ts
    walk c (Record ts)  = Record $ map (second $ walk c) ts
    walk c (Variant ts) = Variant $ Map.map (walk c) ts

substTop :: (Type, Type) -> Type
substTop = subst 0 . shift 1 *** id >>> app >>> shift (-1)

instance Show Type where
  show = display

instance Show TypeId where
  show = display

class Display a where
  display :: a -> String

-- | @display ty@ displays @ty@ as a string. The current implementation is
-- conservative in associativity and comma-separation.
instance Display Type where
  display Bool         = "Bool"
  display Int          = "Int"
  display Unit         = "Unit"
  display (Id i)       = display i
  display (t1 :->: t2) = paren $ display t1 ++ " -> " ++ display t2
  display (Var i x n)  = "v" ++ show x
  display (Rec i t)    = paren $ "μ" ++ i ++ "." ++ display t
  display (Tuple ts)   = brack $ foldr (\t s -> display t ++ "," ++ s) "" ts
  display (Record ts)  = brace $ foldr (\(i, t) s -> i ++ "=" ++ display t ++ "," ++ s) "" ts
  display (Variant ts) = angle $ Map.foldrWithKey (\i t s -> i ++ "=" ++ display t ++ "," ++ s) "" ts

-- | @display i@ displays @i@ as a string. The current implementation adopts
-- arbitrary formats.
instance Display TypeId where
  display (IdString s) = "!X" ++ s
  display (Fresh n) = "?X" ++ show n

paren s = "(" ++ s ++ ")"
brack s = "[" ++ s ++ "]"
brace s = "{" ++ s ++ "}"
angle s = "<" ++ s ++ ">"

type Subst = Map.Map TypeId Type

emptySubst :: Subst
emptySubst = Map.empty

(|->) :: TypeId -> Type -> Subst
(|->) = Map.singleton

-- |
-- Composes two @Subst@ from right in series, i.e.
-- (@apply (a \@\@ b) = apply a . apply b@).
(@@) :: Subst -> Subst -> Subst
x @@ y = Map.map (apply x) y `Map.union` x

-- |
-- @merge@ composes two @Subst@ parallel. It is symmetric.
-- It fails if substitutions conflict.
merge :: Subst -> Subst -> Either Error Subst
merge = mergeA
          preserveMissing
          preserveMissing $
          zipWithAMatched . const $
            \a b -> if a == b
                      then pure a
                      else Left $ MergeConflict a b

infixr 6 `merge`

class Types t where
  apply :: Subst -> t -> t
  tv :: t -> Set.Set TypeId

instance Types Type where
  apply s v @ (Id i)   = Map.findWithDefault v i s
  apply s (t1 :->: t2) = apply s t1 :->: apply s t2
  apply s (Rec i t)    = Rec i $ apply s t
  apply s (Tuple ts)   = Tuple $ map (apply s) ts
  apply s (Record ts)  = Record $ map (second $ apply s) ts
  apply s (Variant ts) = Variant $ Map.map (apply s) ts
  apply _ t            = t

  tv (Id i)       = Set.singleton i
  tv (t1 :->: t2) = tv (t1, t2)
  tv (Rec i t)    = tv t
  tv (Tuple ts)   = tv ts
  tv (Record ts)  = foldMap (tv . snd) ts
  tv (Variant ts) = foldMap tv ts
  tv _            = Set.empty

instance Types t => Types [t] where
  apply = map . apply
  tv = foldMap tv

instance (Ord t, Types t) => Types (Set.Set t) where
  apply = Set.map . apply
  tv = foldMap tv

instance (Types t1, Types t2) => Types (t1, t2) where
  apply s = apply s *** apply s
  tv (x, y) = tv x `Set.union` tv y

type Constraints = Set.Set (Type, Type)

fromList :: [(Type, Type)] -> Constraints
fromList = Set.fromList

data Error
  = Unify Type Type -- ^ @Unify t1 t2@ indicates that @t1@ and @t2@ does not unify..
  | MergeConflict Type Type
  deriving (Eq, Show)

-- | @unify cs@ returns the most general unifier on 'Constraints'.
unify :: Constraints -> Either Error Subst
unify cs = execStateT (mapM_ f cs) emptySubst
  where
    f :: (Type, Type) -> StateT Subst (Either Error) ()
    f (t1, t2) = do
      f <- apply <$> get
      s <- lift $ f t1 `mgu` f t2
      modify $ (s @@)

-- | @mgu ty1 ty2@ returns the most general unifier on two types.
mgu :: Type -> Type -> Either Error Subst
mgu (Id i) t = varBind i t
mgu t (Id i) = varBind i t
mgu (tyS1 :->: tyS2) (tyT1 :->: tyT2) = do
  s <- mgu tyS1 tyT1
  t <- apply s tyS2 `mgu` apply s tyT2
  return $ t @@ s
mgu t1 t2
  | t1 == t2  = return emptySubst
  | otherwise = Left $ Unify t1 t2

varBind :: TypeId -> Type -> Either Error Subst
varBind i1 (Id i2) | i1 == i2 = return emptySubst
varBind i t = return $ Map.singleton i t
