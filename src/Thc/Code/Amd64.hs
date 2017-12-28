module Thc.Code.Amd64
  ( updateContext
  ) where

import qualified Data.ByteString.Lazy as B
import Data.Word

import Thc.Asm
import Thc.Code

updateContext :: Context -> Context
updateContext ctx = ctx { cpu = f }
  where
    f c
      | c == Amd64 = return . fromAsm
      | otherwise  = cpu ctx c

fromAsm :: Asm -> Code
fromAsm (Return' l) = ret l

-- FIXME:
-- Use exit syscall to exit with code: False = 80, True = 81.
-- Note that syscall numbers depend on OS.
ret :: Literal -> Code
ret l = B.concat
  [ syscallNumber
  , B.pack $ 0xbf : encodeBits v ++ [0x0f, 0x05]
  ]
  where
    v :: Word32
    v = fromIntegral $ case l of
      Bool b -> 80 + fromEnum b
      Int i -> i

    -- | TODO: This depends on System V ABI and XNU.
    syscallNumber :: B.ByteString
    syscallNumber = B.pack [0xb8, 1, 0, 0, 2]
