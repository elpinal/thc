module Thc.Expr.Indexed where

data Term =
    Var String Int Int
  | Abs String Term
  | App Term Term

shift :: Int -> Term -> Term
shift d t = walk 0 t
  where
    walk :: Int -> Term -> Term
    walk c (Var i x n)
      | x >= c    = Var i (x + d) (n + d) -- free
      | otherwise = Var i x $ n + d       -- bound
    walk c (Abs i t') = Abs i $ walk (c + 1) t'
    walk c (App t1 t2) = App (walk c t1) (walk c t2)
