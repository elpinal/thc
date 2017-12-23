module Thc.Expr where

data Term =
    Var String
  | Abs String Term
  | App Term Term
