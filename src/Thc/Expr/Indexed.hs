module Thc.Expr.Indexed where

data Term =
    Var Int Int String
  | Abs String Term
  | App Term Term

shift :: Int -> Term -> Term
shift d t = walk 0 t
  where
    walk :: Int -> Term -> Term
    walk c (Var x n i)
      | x >= c    = Var (x + d) (n + d) i -- free
      | otherwise = Var x (n + d) i
    walk c (Abs i t') = Abs i $ walk (c + 1) t'
    walk c (App t1 t2) = App (walk c t1) (walk c t2)
