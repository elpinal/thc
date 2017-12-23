module Thc.Tac where

import qualified Thc.Expr as Expr

data Tac = Ret Val
  deriving (Eq, Show)

data Val = Var String
  deriving (Eq, Show)

data FromExprError = OnlyAbs
  deriving (Eq, Show)

fromExpr :: Expr.Term -> Either FromExprError Tac
fromExpr (Expr.Var i) = Right . Ret $ Var i
fromExpr (Expr.Abs i e) = Left OnlyAbs
fromExpr (Expr.App (Expr.Abs i e1) e2) = fromExpr $ replace e1 i e2

replace :: Expr.Term -> String -> Expr.Term -> Expr.Term
replace v @ (Expr.Var i) r e2
  | i == r    = e2
  | otherwise = v
replace a @ (Expr.Abs i e1) r e2
  | i == r    = a
  | otherwise = Expr.Abs i $ replace e1 r e2
replace a @ (Expr.App e1 e2) r e3 = Expr.App (replace e1 r e3) (replace e2 r e3)
