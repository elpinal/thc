module Thc.Tac where

import qualified Thc.Expr.Indexed as I

data Tac = Return I.Literal
  deriving (Eq, Show)

type Literal = I.Literal

fromLit :: Literal -> Tac
fromLit = Return
