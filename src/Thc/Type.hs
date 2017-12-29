module Thc.Type
  ( Type(..)
  ) where

data Type =
    Bool
  | Int
  | Type :->: Type
