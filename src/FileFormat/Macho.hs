{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module FileFormat.Macho where

import Data.Bits
import Data.Word

class Encode a where
  encode :: a -> [Word8]

instance (FiniteBits a, Integral a) => Encode a where
  encode n = map (fromIntegral . (.&. 0xff)) . take (l `div` 8) $ iterate shiftR8 n
    where
      l :: Int
      l = finiteBitSize n

      shiftR8 :: Bits a => a -> a
      shiftR8 = flip shiftR 8

magicNumber64 :: Word32
magicNumber64 = 0xfeedfacf

abi64 :: Word32
abi64 = 0x01000000

amd64 :: Word32
amd64 = 7 .|. abi64

amd64All :: Word32
amd64All = 3

data FileType =
    Object
  | Execute
  deriving Enum

instance Encode FileType where
  encode f = encode $ (2 :: Word32) ^ fromIntegral (fromEnum f)

lcSegment64 :: Word32
lcSegment64 = 0x19

lcSymtab :: Word32
lcSymtab = 0x02

lcDysymtab :: Word32
lcDysymtab = 0x0b

lcUuid :: Word32
lcUuid = 0x1b
