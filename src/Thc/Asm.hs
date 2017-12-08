module Thc.Asm
  ( Asm
  , Inst(..)
  , fromTac
  , Tac.Val(..)
  ) where

import Data.Bifunctor

import qualified Thc.Tac as Tac

type Asm = ([Inst], [String])

data Inst = Ret Tac.Val
  deriving (Eq, Show)

data Loc = StringTable Int
         | Static Int

fromTac :: Tac.Tac -> Asm -> Asm
fromTac (Tac.Ret v) = retv v

retv v = first (++ [Ret v])
