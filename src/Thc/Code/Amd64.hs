module Thc.Code.Amd64 where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
import Data.Word

import Thc.Asm
import Thc.Code

fromAsm :: Asm -> Code
fromAsm (Ret l, x) = ret l x

ret :: Loc -> String -> Code
ret (StringTable n) x = C.pack x
