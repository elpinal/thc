module FileFormat.Macho where

import Data.Bits
import Data.Word

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
