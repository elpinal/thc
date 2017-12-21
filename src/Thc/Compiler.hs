module Thc.Compiler where

import Thc.Asm
import Thc.Code
import qualified Thc.Code.Amd64 as Amd64
import Thc.Expr
import Thc.Tac

coreContext :: Context
coreContext = Amd64.updateContext context

compile :: Term -> OS -> CPU -> Either Error Code
compile = compileWithContext coreContext

compileWithContext :: Context -> Term -> OS -> CPU -> Either Error Code
compileWithContext ctx t o c = encode ctx (fromTac $ fromExpr t) o c
