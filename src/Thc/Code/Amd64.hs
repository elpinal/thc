module Thc.Code.Amd64 where

import Thc.Code

type Amd64 a = a

fromAsm :: Asm -> Amd64 Code
