module Thc.Asm
  ( Asm(..)
  , fromTac
  , Tac.Val(..)
  ) where

import qualified Thc.Tac as Tac

data Asm

data Inst = Ret Tac.Val
  deriving (Eq, Show)

data Loc = StringTable Int
         | Static Int

fromTac :: Tac.Tac -> Asm
fromTac (Tac.Ret v) = Ret v
