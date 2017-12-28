module Thc.Tac
  ( Tac(..)
  , fromLit
  ) where

import Thc.Expr.Indexed

data Tac = Return Literal
  deriving (Eq, Show)

fromLit :: Literal -> Tac
fromLit = Return
