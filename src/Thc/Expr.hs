module Thc.Expr where

import qualified Data.Set as Set

data Term =
    Var String
  | Abs String Term
  | App Term Term

-- This is an useful function. Because Thc is a compiler, this is not
-- necessary for the use.
eval :: Term -> Term
eval t = maybe t eval $ eval1 t

eval1 :: Term -> Maybe Term
eval1 (App (Abs i t1) t2) = return $ subst i t2 t1
eval1 (App t1 t2) = flip App t2 <$> eval1 t1
eval1 _ = Nothing

subst :: String -> Term -> Term -> Term
subst i t (Var i')
  | i == i'   = t
  | otherwise = Var i'
subst i t (App t1 t2) = subst i t t1 `App` subst i t t2
subst i t (Abs i' t')
  | i == i' = Abs i' t'
  | i' `Set.notMember` fv t = Abs i' $ subst i t t'

fv :: Term -> Set.Set String
fv (Var i) = Set.singleton i
fv (Abs i t) = Set.delete i $ fv t
fv (App t1 t2) = fv t1 `Set.union` fv t2
