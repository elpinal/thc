module Thc.Compiler where

import Thc.Asm
import Thc.Code
import Thc.Expr
import Thc.Tac

compile :: Context -> Term -> OS -> CPU -> Either Error Code
compile ctx t o c = encode ctx (fromTac $ fromExpr t) o c
