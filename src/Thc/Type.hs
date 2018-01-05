module Thc.Type
  ( Type(..)
  , variant
  , subtype
  ) where

import qualified Data.Map.Lazy as Map

data Type =
    Bool
  | Int
  | Unit
  | Type :->: Type
  | Tuple [Type]
  | Record [(String, Type)]
  | Variant (Map.Map String Type)
  deriving (Eq, Show)

infixr 9 :->:

-- | @variant xs@ creates a new 'Variant' from @xs@.
variant :: [(String, Type)] -> Type
variant = Variant . Map.fromList

-- |
-- @subtype s t@ tests whether @s@ is subtype of @t@.
-- It returns @True@ if @s == t@.
subtype :: Type -> Type -> Bool
subtype (Variant ss) (Variant ts) = Map.isSubmapOf ss ts
subtype s t = s == t
