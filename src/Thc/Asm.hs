module Thc.Asm
  ( Asm
  , Tac(..)
  , Literal(..)
  , fromTac
  ) where

import Thc.Expr.Indexed
import Thc.Tac

type Asm = Tac

fromTac :: Tac -> Asm
fromTac = id
