module FileFormat.Macho where

magicNumber64 = 0xfeedfacf

abi64 = 0x01000000

amd64 = 7 .|. abi64
