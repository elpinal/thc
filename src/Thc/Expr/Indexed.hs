module Thc.Expr.Indexed where

import Control.Arrow

data Term =
    Var String Int Int
  | Abs String Term
  | App Term Term
  | Lit Literal
  deriving Show

data Literal =
    Bool Bool
  | Int Int
  deriving Show

shift :: Int -> Term -> Term
shift d t = walk 0 t
  where
    walk :: Int -> Term -> Term
    walk c (Var i x n)
      | x >= c    = Var i (x + d) (n + d) -- free
      | otherwise = Var i x $ n + d       -- bound
    walk c (Abs i t') = Abs i $ walk (c + 1) t'
    walk c (App t1 t2) = App (walk c t1) (walk c t2)
    walk c l @ (Lit _) = l

subst :: Int -> Term -> Term -> Term
subst j s t = walk 0 t
  where
    walk :: Int -> Term -> Term
    walk c t' @ (Var i x n)
      | x == j + c = shift c s
      | otherwise  = t'
    walk c (Abs i t') = Abs i $ walk (c + 1) t'
    walk c (App t1 t2) = App (walk c t1) (walk c t2)
    walk c l @ (Lit _) = l

-- subst
substTop :: (Term, Term) -> Term
substTop = subst 0 . shift 1 *** id >>> app >>> shift (-1)

-- eval
eval :: Term -> Term
eval t = maybe t eval $ eval1 t

eval1 :: Term -> Maybe Term
eval1 (App (Abs _ t1) t2) = return $ substTop (t2, t1)
eval1 (App t1 t2) = flip App t2 <$> eval1 t1
eval1 _ = Nothing

fromLit :: Term -> Maybe Literal
fromLit (Lit l) = return l
fromLit _ = Nothing