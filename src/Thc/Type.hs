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
