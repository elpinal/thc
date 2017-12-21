module Thc.Compiler where

import Thc.Asm
import Thc.Code
import Thc.Expr
import Thc.Tac

compile :: Machine m => Term -> m Code
compile = fromAsm . fromTac . fromExpr
