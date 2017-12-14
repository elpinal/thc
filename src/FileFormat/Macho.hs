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

emptyFile :: File
emptyFile = File
  { header   = header64
  , segments = [segment]
  }

instance Encode File where
  encode File {header = h, segments = ss} = encodeHeader ss (length bs) h ++ bs
    where
      bs :: [Word8]
      bs = foldl f [] ss

      f :: [Word8] -> Segment -> [Word8]
      f acc s = acc ++ encodeSegment dataOffset (fromIntegral $ length acc) s

      dataOffset :: Word32
      dataOffset = sum $ map (\Segment {sections = secs} -> segmentSize + sectionSize * fromIntegral (length secs)) ss

data Header = Header
  { magic      :: Word32
  , cputype    :: Word32
  , cpusubtype :: Word32
  , filetype   :: FileType
  , hflags     :: Word32
  }

header64 :: Header
header64 = Header
  { magic      = magicNumber64
  , cputype    = amd64
  , cpusubtype = amd64All
  , filetype   = Execute
  , hflags     = 0
  }

encodeHeader :: [Segment] -> Int -> Header -> [Word8]
encodeHeader ss l Header
  { magic      = m
  , cputype    = c
  , cpusubtype = cs
  , filetype   = f
  , hflags     = h
  } = concatMap encodeBits [m, c, cs] ++ encode f ++ ncmds ++ encodeBits sizeofcmds ++ encodeBits h ++ reserved
  where
    reserved :: [Word8]
    reserved = [0, 0, 0, 0]

    ncmds :: [Word8]
    ncmds = encodeBits (fromIntegral $ length ss :: Word32)

    sizeofcmds :: Word32
    sizeofcmds = fromIntegral l

data Segment = Segment
  { segname  :: String
  , maddr    :: Word64
  , msize    :: Word64
  , maxprot  :: [Prot]
  , initprot :: [Prot]
  , sections :: [Section]
  , segflags :: Word32
  }

segment :: Segment
segment = Segment
  { segname  = ""
  , maddr    = 0
  , msize    = 0
  , maxprot  = allProt
  , initprot = allProt
  , sections = []
  , segflags = 0
  }

textSegment :: [Word8] -> Segment
textSegment text = Segment
  { segname  = "__TEXT"
  , maddr    = 0
  , msize    = fromIntegral $ length text
  , maxprot  = allProt
  , initprot = [Readable, Executable]
  , sections = [textSection text]
  , segflags = 0
  }

segment64 :: Word32
segment64 = 0x19

-- | The size of @segment_command_64@.
segmentSize :: Word32
segmentSize = 72

encodeSegment :: Word32 -> Word32 -> Segment -> [Word8]
encodeSegment dataOffset offset Segment
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
      ++ encodeBits (nsects :: Word32)
      ++ encodeBits fs
      ++ ffu
  where
    nsects :: Num a => a
    nsects = fromIntegral $ length ss

    ffu :: [Word8]
    ffu = fst $ foldl f ([], dataOffset) ss

    f :: ([Word8], Word32) -> Section -> ([Word8], Word32)
    f (acc, off) s = (acc ++ encodeSection n (fromIntegral $ length acc) s, sizeOfSection s)

    -- offsets :: [Word32]
    -- offsets = take nsects . iterate (+ sectionSize) $ offset + segmentSize

sizeOfSection :: Section -> Word32
sizeOfSection Section {size = s} = fromIntegral s

data Section = Section
  { secname  :: String
  , addr     :: Word64
  , size     :: Word64
  , align    :: Word32
  , secflags :: Word32
  }

textSection :: [Word8] -> Section
textSection text = Section
  { secname  = "__text"
  , addr     = 0
  , size     = fromIntegral $ length text
  , align    = 0
  , secflags = 0
  }

-- | The size of @section_64@.
sectionSize :: Word32
sectionSize = 80

-- TODO: Consider 'offset' field.
encodeSection :: String -> Word32 -> Section -> [Word8]
encodeSection segn dataOffset Section
  { secname  = n
  , addr     = a
  , size     = s
  , align    = al
  , secflags = fs
  } = concatMap encode [n, segn] ++ concatMap encodeBits [a, s] ++ concatMap encodeBits [dataOffset, al, 0, 0, fs] ++ reserved
  where
    reserved :: [Word8]
    reserved = concatMap encodeBits $ replicate 3 (0x00 :: Word32)

data Prot =
    Readable
  | Writable
  | Executable
  deriving Enum

instance Encode [Prot] where
  encode = encodeBits . foldr (.|.) 0 . map (\p -> (2 :: Word32) ^ fromEnum p)

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
