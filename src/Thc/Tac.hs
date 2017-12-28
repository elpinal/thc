module Thc.Tac where

import Control.Monad.State.Lazy

import qualified Thc.Expr as Expr
import qualified Thc.Expr.Indexed as I

data Tac'' = Return I.Literal
  deriving (Eq, Show)

type Literal = I.Literal

fromLit :: Literal -> Tac''
fromLit = Return
