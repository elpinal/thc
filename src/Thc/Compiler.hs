module Thc.Compiler where

import qualified OS.Darwin as Darwin
import Thc.Asm
import Thc.Code
import qualified Thc.Code.Amd64 as Amd64
import Thc.Expr
import Thc.Tac

coreContext :: Context
coreContext = Darwin.updateContext . Amd64.updateContext $ context

data CompileError =
    FromExpr FromExprError
  | FromAsm Error
  deriving (Eq, Show)

compile :: Term -> OS -> CPU -> Either CompileError Code
compile = compileWithContext coreContext

compileWithContext :: Context -> Term -> OS -> CPU -> Either Error Code
compileWithContext ctx t o c = encodeFromAsm ctx (fromTac $ fromExpr t) o c
