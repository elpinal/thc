{-|
A module to deal with three address code.
-}
module Thc.Tac
  ( Tac(..)
  , fromLit
  ) where

import Thc.Expr.Indexed

data Tac = Return Literal
  deriving (Eq, Show)

fromLit :: Literal -> Tac
fromLit = Return
