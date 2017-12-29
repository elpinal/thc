module Thc.Type
  ( Type(..)
  ) where

data Type =
    Bool
  | Int
  | Unit
  | Type :->: Type
  deriving (Eq, Show)

infixr 9 :->:
