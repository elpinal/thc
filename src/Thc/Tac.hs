module Thc.Tac where

import Control.Monad.State.Lazy

import qualified Thc.Expr as Expr
import qualified Thc.Expr.Indexed as I

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

-- FIXME: Use correct Beta reduction
replace :: Expr.Term -> String -> Expr.Term -> Expr.Term
replace v @ (Expr.Var i) r e2
  | i == r    = e2
  | otherwise = v
replace a @ (Expr.Abs i e1) r e2
  | i == r    = a
  | otherwise = Expr.Abs i $ replace e1 r e2
replace a @ (Expr.App e1 e2) r e3 = Expr.App (replace e1 r e3) (replace e2 r e3)

-------------------

type Tac' = ([Inst], [FnDecl])

data Inst =
    Assign String String
  | App String String
  | Return String
  deriving (Eq, Show)

data FnDecl = FnDecl String String [Inst]
  deriving (Eq, Show)

data FromExprError' = UndefinedVariable String
  deriving (Eq, Show)

fromExpr' :: Expr.Term -> StateT Int (Either FromExprError') (String, [Inst], [FnDecl])

fromExpr' (Expr.Var i) = return (i, [], [])

fromExpr' (Expr.Abs i t) = do
  fi <- ("f" ++) . show <$> freshName -- FIXME: be likely to conflict
  (a, is, fs) <- fromExpr' t
  return (fi, is, fs ++ [FnDecl fi i [Return a]])

fromExpr' (Expr.App t1 t2) = do
  i <- ("a" ++) . show <$> freshName -- FIXME: be likely to conflict
  (a, is1, fs1) <- fromExpr' t1
  (b, is2, fs2) <- fromExpr' t2
  return (i, is1 ++ is2 ++ [App a b], fs1 ++ fs2)

freshName :: Monad m => StateT Int m Int
freshName = do
  n <- get
  put $ n + 1
  return n

-------------------

data Tac'' = Return' I.Literal

fromLit :: I.Literal -> Tac''
fromLit = Return'
