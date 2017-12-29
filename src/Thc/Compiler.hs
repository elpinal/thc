module Thc.Compiler where

import Data.Bifunctor

import           Thc.Asm
import           Thc.Code
import qualified Thc.Expr as E
import           Thc.Expr.Indexed
import           Thc.Tac

import qualified OS.Darwin as Darwin
import qualified Thc.Code.Amd64 as Amd64

coreContext :: Context
coreContext = Darwin.updateContext . Amd64.updateContext $ context

data CompileError =
    NotLit
  | Unbound
  | FromAsm Error
  deriving (Eq, Show)

compile :: E.Term -> OS -> CPU -> Either CompileError Code
compile = compileWithContext coreContext

compileWithContext :: Context -> E.Term -> OS -> CPU -> Either CompileError Code
compileWithContext ctx t o c = assemble . fromTac =<< genTac =<< genIndexed t
  where
    genIndexed :: E.Term -> Either CompileError Term
    genIndexed = maybe (Left Unbound) return . fromNamed

    genTac :: Term -> Either CompileError Tac
    genTac = maybe (Left NotLit) (return . fromLit) . fromLiteral . eval

    assemble :: Asm -> Either CompileError Code
    assemble = first FromAsm . encodeFromAsm ctx o c
