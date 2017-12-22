module Thc.Code where

import qualified Data.ByteString.Lazy as B

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
