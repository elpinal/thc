module Thc.Type
  ( Type(..)
  , variant
  ) where

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
