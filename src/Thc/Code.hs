module Thc.Code where

import Data.Word

import Thc.Asm

type Code = [Word8]

class Machine m where
  fromAsm :: Asm -> m Code

data Loc = StringTable Int
