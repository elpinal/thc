module Thc.Asm
  ( Asm
  , Inst(..)
  , fromTac
  , Tac.Val(..)
  ) where

import qualified Thc.Tac as Tac

type Asm = [Inst]

data Inst = Ret Tac.Val
  deriving (Eq, Show)

data Loc = StringTable Int
         | Static Int

fromTac :: Tac.Tac -> Asm -> Asm
fromTac (Tac.Ret v) asm = asm ++ [Ret v]
