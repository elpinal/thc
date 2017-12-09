module FileFormat.Macho where

import Data.Bits
import Data.Word

magicNumber64 = 0xfeedfacf

abi64 = 0x01000000

amd64 :: Word32
amd64 = 7 .|. abi64

amd64All = 3

type FileType = Int

object = 1
execute = 2
