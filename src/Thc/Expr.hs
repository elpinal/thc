module Thc.Expr
  ( Term(..)
  , Literal(..)
  ) where

data Term =
    Var String
  | Abs String Term
  | App Term Term
  | Lit Literal

data Literal =
    Bool Bool
  | Int Int
  deriving (Eq, Show)
