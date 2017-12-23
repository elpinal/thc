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
