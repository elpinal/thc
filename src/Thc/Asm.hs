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

type AsmTransformer = Asm -> Asm

fromTac :: Tac.Tac -> AsmTransformer
fromTac (Tac.Ret v) = retv v

retv :: Tac.Val -> AsmTransformer
retv (Tac.Var i) = bimap (const $ Ret $ StringTable 0) (const i)
