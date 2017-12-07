module Thc.Code.Amd64 where

import Thc.Asm
import Thc.Code

data Amd64 a = Amd64 a

instance Machine Amd64 where
  fromAsm :: Asm -> Amd64 Code
  fromAsm (Ret v) = Amd64 []
