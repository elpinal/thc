module Thc.Expr where

data Term =
    Var String
  | Abs String Term
  | App Term Term

-- This is an useful function. Because Thc is a compiler, this is not
-- necessary for the use.
eval :: Term -> Term
eval t = case eval1 t of
  Just t' -> eval t'
  Nothing -> t

eval1 :: Term -> Maybe Term
eval1 (App (Abs i t1) t2) = return $ subst i t2 t1
eval1 (App t1 t2) = flip App t2 <$> eval1 t1
eval1 _ = Nothing

subst :: String -> Term -> Term -> Term
subst i t2 t1 = undefined
