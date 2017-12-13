{-# LANGUAGE FlexibleInstances #-}

module FileFormat.Macho where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

import Data.Bits
import Data.Word

class Encode a where
  encode :: a -> [Word8]

instance Encode String where
  encode = fillString16 . B.unpack . C.pack

fillString16 :: [Word8] -> [Word8]
fillString16 xs
  | length xs > 16 = error $ "the string is too long; the max is 16 bytes, but got " ++ show (length xs)
  | otherwise      = xs ++ replicate (16 - length xs) 0x00

encodeBits :: (FiniteBits a, Integral a) => a -> [Word8]
encodeBits n = map (fromIntegral . (.&. 0xff)) . take b $ iterate shiftR8 n
  where
    b :: Int
    b = finiteBitSize n `div` 8

    shiftR8 :: Bits a => a -> a
    shiftR8 = flip shiftR 8

data File = File
  { header   :: Header
  , segments :: [Segment]
  }

instance Encode File where
  encode File {header = h, segments = ss} = encodeHeader h ++ concatMap encodeSegment ss

data Header = Header
  { magic      :: Word32
  , cputype    :: Word32
  , cpusubtype :: Word32
  , filetype   :: FileType
  , hflags     :: Word32
  }

encodeHeader :: Header -> [Word8]
encodeHeader Header
  { magic      = m
  , cputype    = c
  , cpusubtype = cs
  , filetype   = f
  , hflags     = h
  } = concatMap encodeBits [m, c, cs] ++ encode f ++ encodeBits h ++ reserved
  where
    reserved :: [Word8]
    reserved = [0, 0, 0, 0]

data Segment = Segment
  { segname  :: String
  , maddr    :: Word64
  , msize    :: Word64
  , maxprot  :: [Prot]
  , initprot :: [Prot]
  , sections :: [Section]
  , segflags :: Word32
  }

segment64 :: Word32
segment64 = 0x19

-- | The size of @segment_command_64@.
segmentSize :: Word32
segmentSize = 72

encodeSegment :: Segment -> [Word8]
encodeSegment Segment
  { segname  = n
  , maddr    = a
  , msize    = s
  , maxprot  = mp
  , initprot = ip
  , sections = ss
  , segflags = fs
  } = concatMap encodeBits [segment64, segmentSize + sectionSize * nsects]
      ++ encode n
      ++ concatMap encodeBits [a, s]
      ++ concatMap encodeBits [0, 0 :: Word64] -- Use zeros for the present
      ++ concatMap encode [mp, ip]
      ++ encodeBits nsects
      ++ encodeBits fs
      ++ concatMap (encodeSection n) ss
  where
    nsects :: Word32
    nsects = fromIntegral $ length ss

data Section = Section
  { secname  :: String
  , addr     :: Word64
  , size     :: Word64
  , align    :: Word32
  , secflags :: Word32
  }

-- | The size of @section_64@.
sectionSize :: Word32
sectionSize = 80

-- TODO: Consider 'offset' field.
encodeSection :: String -> Section -> [Word8]
encodeSection segn Section
  { secname  = n
  , addr     = a
  , size     = s
  , align    = al
  , secflags = fs
  } = concatMap encode [n, segn] ++ concatMap encodeBits [a, s] ++ concatMap encodeBits [al, 0, 0, fs] ++ reserved
  where
    reserved :: [Word8]
    reserved = concatMap encodeBits $ replicate 3 (0x00 :: Word32)

data Prot =
    Readable
  | Writable
  | Executable
  deriving Enum

instance Encode [Prot] where
  encode = encodeBits . foldr (.&.) 0 . map (\p -> (2 :: Word32) ^ fromEnum p)

allProt :: [Prot]
allProt = [Readable, Writable, Executable]

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
  encode f = encodeBits $ (2 :: Word32) ^ fromIntegral (fromEnum f)

lcSegment64 :: Word32
lcSegment64 = 0x19

lcSymtab :: Word32
lcSymtab = 0x02

lcDysymtab :: Word32
lcDysymtab = 0x0b

lcUuid :: Word32
lcUuid = 0x1b
