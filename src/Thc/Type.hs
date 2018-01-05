module Thc.Type
  ( Type(..)
  ) where

data Type =
    Bool
  | Int
  | Unit
  | Type :->: Type
  | Tuple [Type]
  | Record [(String, Type)]
  | Variant [(String, Type)]
  deriving (Eq, Show)

infixr 9 :->:

-- |
-- @subtype s t@ tests whether @s@ is subtype of @t@.
-- It returns @True@ if @s == t@.
subtype :: Type -> Type -> Bool
subtype s t
  | s == t = True
