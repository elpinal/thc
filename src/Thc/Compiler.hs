module Thc.Compiler where

import Control.Monad
import Data.Bifunctor

import qualified OS.Darwin as Darwin
import Thc.Asm
import Thc.Code
import qualified Thc.Code.Amd64 as Amd64
import Thc.Expr.Indexed
import Thc.Tac

coreContext :: Context
coreContext = Darwin.updateContext . Amd64.updateContext $ context

data CompileError =
    NotLit
  | FromAsm Error
  deriving (Eq, Show)

compile :: Term -> OS -> CPU -> Either CompileError Code
compile = compileWithContext coreContext

compileWithContext :: Context -> Term -> OS -> CPU -> Either CompileError Code
compileWithContext ctx t o c = assemble . fromTac' =<< genTac t
  where
    genTac :: Term -> Either CompileError Tac''
    genTac = maybe (Left NotLit) (return . fromLit) . fromLiteral . eval

    assemble :: Asm -> Either CompileError Code
    assemble = first FromAsm . encodeFromAsm ctx o c
