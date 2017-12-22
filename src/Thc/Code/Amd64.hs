module Thc.Code.Amd64
  ( updateContext
  ) where

import qualified Data.ByteString.Lazy as B
import Data.Word

import Thc.Asm
import Thc.Code

updateContext :: Context -> Context
updateContext ctx = ctx { cpu = \c -> if c == Amd64 then return . fromAsm else cpu ctx c }

fromAsm :: Asm -> Code
fromAsm (Ret l, x) = ret l x

-- FIXME: Use exit syscall to exit with code: length x.
--        Note that syscall numbers depend on OS.
ret :: Loc -> String -> Code
ret (StringTable n) x = syscallNumber `B.append` B.singleton 0xbf `B.append` B.pack (encodeBits v) `B.append` B.pack [0x0f, 0x05]
  where
    v :: Word32
    v = fromIntegral $ length x

    syscallNumber :: B.ByteString
    syscallNumber = B.pack [0xb8, 1, 0, 0, 2]
