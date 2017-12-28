module Thc.Asm
  ( Asm
  , Asm'
  , Tac.Tac''(..)
  , I.Literal(..)
  , Inst(..)
  , Loc(..)
  , fromTac
  , fromTac'
  , Tac.Val(..)
  ) where

import qualified Thc.Expr.Indexed as I
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

-------------------

type Asm' = Tac.Tac''

fromTac' :: Tac.Tac'' -> Asm'
fromTac' = id
