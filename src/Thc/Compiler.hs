module Thc.Compiler
  ( compile
  , CompileError(..)
  ) where

import Data.Bifunctor

import           Thc.Asm
import           Thc.Code
import qualified Thc.Expr as E
import           Thc.Expr.Indexed
import           Thc.Tac
import qualified Thc.Type as T

import qualified OS.Darwin as Darwin
import qualified Thc.Code.Amd64 as Amd64

coreContext :: Context
coreContext = Darwin.updateContext . Amd64.updateContext $ context

data CompileError =
    NotLit
  | Eval EvalError
  | Type TypeError
  | NonTypable
  | FromAsm Error
  deriving (Eq, Show)

fromEvalError :: EvalError -> CompileError
fromEvalError = Eval

fromTypeError :: TypeError -> CompileError
fromTypeError = Type

compile :: E.Term -> OS -> CPU -> Either CompileError Code
compile = compileWithContext coreContext

compileWithContext :: Context -> E.Term -> OS -> CPU -> Either CompileError Code
compileWithContext ctx t o c = do
  tm <- genIndexed t
  verifyType tm
  tac <- genTac tm
  assemble . fromTac $ tac
  where
    genIndexed :: E.Term -> Either CompileError Term
    genIndexed = first fromEvalError . fromNamed

    verifyType :: Term -> Either CompileError T.Type
    verifyType = first fromTypeError . head . typeOf -- TODO: the use of 'head' is unsafe!

    genTac :: Term -> Either CompileError Tac
    genTac = fmap fromLit . try' fromLiteral . eval

    assemble :: Asm -> Either CompileError Code
    assemble = first FromAsm . encodeFromAsm ctx o c

class Try a where
  try :: Maybe a -> Either CompileError a

try' :: Try a => (b -> Maybe a) -> b -> Either CompileError a
try' f = try . f

instance Try Literal where
  try = maybe (Left NotLit) return
