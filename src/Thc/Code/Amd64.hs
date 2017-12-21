module Thc.Code.Amd64
  ( updateContext
  ) where

import qualified Data.ByteString.Lazy.Char8 as C

import Thc.Asm
import Thc.Code

updateContext :: Context -> Context
updateContext c = c { cpu = \Amd64 -> return . fromAsm }

fromAsm :: Asm -> Code
fromAsm (Ret l, x) = ret l x

ret :: Loc -> String -> Code
ret (StringTable n) x = C.pack x
