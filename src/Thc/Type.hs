module Thc.Type
  ( Type(..)
  , TypeId(IdString)
  , variant
  , substTop
  ) where

import Control.Arrow
import Control.Monad.Trans.State.Lazy
import qualified Data.Map.Lazy as Map
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
  deriving Eq

infixr 9 :->:

data TypeId
  = IdString String
  | Fresh Int
  deriving (Eq, Ord)

freshVar :: State Int TypeId
freshVar = state $ \n -> (Fresh n, n + 1)

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

-- |
-- Composes two @Subst@ from right in series, i.e.
-- (@apply (a \@\@ b) = apply a . apply b@).
(@@) :: Subst -> Subst -> Subst
x @@ y = Map.map (apply x) y `Map.union` x

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
