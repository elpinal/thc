module Thc.Asm
  ( Asm
  , Inst(..)
  , Loc(..)
  , fromTac
  , Tac.Val(..)
  ) where

import Data.Bifunctor

import qualified Thc.Tac as Tac

type Asm = (Inst, String)

data Inst = Ret Loc
  deriving (Eq, Show)

data Loc = StringTable Int
  deriving (Eq, Show)

fromTac :: Tac.Tac -> Asm
fromTac (Tac.Ret v) = retv v

retv :: Tac.Val -> Asm
retv (Tac.Var i) = (Ret $ StringTable 0, i)
