module Thc.Compiler where

import Data.Bifunctor

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

compileWithContext :: Context -> Term -> OS -> CPU -> Either CompileError Code
compileWithContext ctx t o c = genTac t >>= assemble . fromTac
  where
    genTac :: Term -> Either CompileError Tac
    genTac = first FromExpr . fromExpr

    assemble :: Asm -> Either CompileError Code
    assemble = first FromAsm . encodeFromAsm ctx o c
