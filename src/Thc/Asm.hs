module Thc.Asm
  ( Asm(..)
  , fromTac
  , Tac.Val(..)
  ) where

import qualified Thc.Tac as Tac

data Asm = Ret Tac.Val
  deriving (Eq, Show)

fromTac :: Tac.Tac -> Asm
fromTac (Tac.Ret v) = Ret v
