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

type AsmTransformer = Asm -> Asm

fromTac :: Tac.Tac -> AsmTransformer
fromTac (Tac.Ret v) = retv v

retv :: Tac.Val -> AsmTransformer
retv v = first (++ [Ret v])
