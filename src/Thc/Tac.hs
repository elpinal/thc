module Thc.Tac where

import qualified Thc.Expr as Expr

data Tac = Ret Val
  deriving (Eq, Show)

data Val = Var String
  deriving (Eq, Show)

fromExpr :: Expr.Term -> Tac
fromExpr (Expr.Var i) = Ret $ Var i
