module Thc.Code.Amd64 where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
import Data.Word

import Thc.Asm
import Thc.Code

data Amd64 = Amd64 B.ByteString

instance Machine Amd64 where
  fromAsm (Ret l, x) = ret l x
  fromMachine (Amd64 b) = b

ret :: Loc -> String -> Amd64
ret (StringTable n) x = Amd64 $ C.pack x
