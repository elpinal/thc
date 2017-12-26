module Thc.Expr.Indexed where

data Term =
    Var Int Int String
  | Abs String Term
  | App Term Term
