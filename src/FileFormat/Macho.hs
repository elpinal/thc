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

magicNumber64 = 0xfeedfacf

abi64 = 0x01000000

amd64 :: Word32
amd64 = 7 .|. abi64

amd64All = 3

type FileType = Word32

object :: FileType
object = 1

execute :: FileType
execute = 2

lcSegment64 = 0x19

lcSymtab = 0x02

lcDysymtab = 0x0b

lcUuid = 0x1b
