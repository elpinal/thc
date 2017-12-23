module Thc.Code where

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Word

import Thc.Asm

type Code = B.ByteString

data Context = Context
  { cpu :: CPU -> Asm -> Either Error Code
  , os  :: CPU -> OS -> Code -> Either Error Code
  }

data CPU = Amd64
  deriving (Eq, Show)

data OS = Darwin
  deriving (Eq, Show)

context :: Context
context = Context
  { cpu = \c _ -> Left $ NoCPU c
  , os = \_ o _ -> Left $ NoOS o
  }

encodeFromAsm :: Context -> OS -> CPU -> Asm -> Either Error Code
encodeFromAsm Context
  { cpu = cc
  , os = co
  } o c a = do
    text <- cc c a
    co c o text

data Error =
    NoCPU CPU
  | NoOS OS
  deriving (Eq, Show)

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
