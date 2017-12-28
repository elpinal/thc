module Thc.Asm
  ( Asm
  , Tac.Tac''(..)
  , I.Literal(..)
  , fromTac'
  , Tac.Val(..)
  ) where

import qualified Thc.Expr.Indexed as I
import qualified Thc.Tac as Tac

type Asm = Tac.Tac''

fromTac' :: Tac.Tac'' -> Asm
fromTac' = id
