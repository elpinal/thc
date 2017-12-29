module Thc.Type
  ( Type(..)
  ) where

data Type =
    Bool
  | Int
  | Type :->: Type
  deriving (Eq, Show)

infixr 9 :->:
