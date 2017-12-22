module Thc.Compiler where

import qualified OS.Darwin as Darwin
import Thc.Asm
import Thc.Code
import qualified Thc.Code.Amd64 as Amd64
import Thc.Expr
import Thc.Tac

coreContext :: Context
coreContext = Darwin.updateContext . Amd64.updateContext $ context

compile :: Term -> OS -> CPU -> Either Error Code
compile = compileWithContext coreContext

compileWithContext :: Context -> Term -> OS -> CPU -> Either Error Code
compileWithContext ctx t o c = encodeFromAsm ctx (fromTac $ fromExpr t) o c
