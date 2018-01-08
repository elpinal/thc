module Thc.Type
  ( Type(..)
  , variant
  , substTop
  ) where

import Control.Arrow
import qualified Data.Map.Lazy as Map

data Type =
    Bool
  | Int
  | Unit
  | Type :->: Type
  | Var String Int Int
  | Rec String Type
  | Tuple [Type]
  | Record [(String, Type)]
  | Variant (Map.Map String Type)
  deriving (Eq, Show)

infixr 9 :->:

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
    walk c Bool = Bool
    walk c Int = Int
    walk c Unit = Unit
    walk c (t1 :->: t2) = walk c t1 :->: walk c t2
    walk c (Var i x n) = f c i x n
    walk c (Rec i t) = Rec i $ walk (c + 1) t
    walk c (Tuple ts) = Tuple $ map (walk c) ts
    walk c (Record ts) = Record $ map (second $ walk c) ts
    walk c (Variant ts) = Variant $ Map.map (walk c) ts

substTop :: (Type, Type) -> Type
substTop = subst 0 . shift 1 *** id >>> app >>> shift (-1)
