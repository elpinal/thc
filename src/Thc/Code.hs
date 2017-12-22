module Thc.Code where

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Word

import Thc.Asm

type Code = B.ByteString

data Context = Context
  { cpu :: CPU -> Asm -> Either Error Code
  , os  :: OS -> Code -> CPU -> Either Error Code
  }

data CPU = Amd64
  deriving Eq

data OS = Darwin
  deriving Eq

context :: Context
context = Context
  { cpu = \c _ -> Left $ NoCPU c
  , os = \o _ _ -> Left $ NoOS o
  }

encodeFromAsm :: Context -> Asm -> OS -> CPU -> Either Error Code
encodeFromAsm Context
  { cpu = cc
  , os = co
  } a o c = do
    text <- cc c a
    co o text c

data Error =
    NoCPU CPU
  | NoOS OS

-- | The 'Encode' class is used to encode something to bytes.
class Encode a where
  encode :: a -> [Word8]

encodeBits :: (FiniteBits a, Integral a) => a -> [Word8]
encodeBits n = map (fromIntegral . (.&. 0xff)) . take b $ iterate shiftR8 n
  where
    b :: Int
    b = finiteBitSize n `div` 8

    shiftR8 :: Bits a => a -> a
    shiftR8 = flip shiftR 8
