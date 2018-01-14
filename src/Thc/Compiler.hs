module Thc.Compiler
  ( compile
  , CompileError(..)
  ) where

import Control.Monad.Trans.Except
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

compile :: MonadThrowPlus m => E.Term -> OS -> CPU -> m (Either CompileError Code)
compile t os cpu = runExceptT $ compileWithContext coreContext t os cpu

compileWithContext :: MonadThrowPlus m => Context -> E.Term -> OS -> CPU -> ExceptT CompileError m Code
compileWithContext ctx t o c = do
  tm <- genIndexed t
  verifyType tm
  tac <- genTac tm
  assemble . fromTac $ tac
  where
    genIndexed :: Monad m => E.Term -> ExceptT CompileError m Term
    genIndexed = ExceptT . return . first fromEvalError . fromNamed

    verifyType :: MonadThrowPlus m => Term -> ExceptT CompileError m T.Type
    verifyType = ExceptT . fmap (first fromTypeError) . principal

    genTac :: MonadThrowPlus m => Term -> ExceptT CompileError m Tac
    genTac = ExceptT . fmap (fmap fromLit . try' fromLiteral) . eval

    assemble :: MonadThrowPlus m => Asm -> ExceptT CompileError m Code
    assemble = ExceptT . return . first FromAsm . encodeFromAsm ctx o c

class Try a where
  try :: Maybe a -> Either CompileError a

try' :: Try a => (b -> Maybe a) -> b -> Either CompileError a
try' f = try . f

instance Try Literal where
  try = maybe (Left NotLit) return
