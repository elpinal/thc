module Thc.Asm where

data Asm = Copy Loc Loc

data Loc = Mem Int
