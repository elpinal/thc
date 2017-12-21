{-|
This module is provided to deal with Mach-O file format.
-}
{-# LANGUAGE FlexibleInstances #-}

module FileFormat.Macho
  ( executableFromText
  , Encode(..)
  , encodeBits
  , encodeHeader
  , encodeSegment
  , encodeSection
  , encodeThreadState
  , File(..)
  , Header(..)
  , Segment(..)
  , Section(..)
  , ThreadState(..)
  , FileType(..)
  , Prot(..)
  , fillString16
  ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

import Data.Bits
import Data.Word

-- $setup
-- >>> import Test.QuickCheck

-- | The 'Encode' class is used to encode something to bytes.
class Encode a where
  encode :: a -> [Word8]

instance Encode String where
  encode = fillString16 . B.unpack . C.pack

-- | 'fillString16' @xs@ pads @xs@ with zeros to make the length 16 bytes.
--
-- >>> fillString16 [7, 8] == [7, 8] ++ replicate 14 0
-- True
--
-- prop> suchThat arbitrary (\xs -> length xs <= 16) `forAll` (\xs -> length (fillString16 xs) == 16)
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

-- | 'executableFromText' @text@ creates an executable Mach-O binary from 'text'.
executableFromText :: [Word8] -> [Word8]
executableFromText txt = bs ++ spaces
  where
    bs :: [Word8]
    bs = encode file ++ txt

    file :: File
    file = File header64 [pagezero, textSegment txt] threadState

    spaces :: [Word8]
    spaces = replicate (minBytes - length bs) 0x00

    minBytes :: Int
    minBytes = 0x1000

-- | The Mach-O file.
data File = File
  { header   :: Header
  , segments :: [Segment]
  , threadS  :: ThreadState
  }

emptyFile :: File
emptyFile = File
  { header   = header64
  , segments = [segment]
  , threadS = threadState
  }

-- | Get the length of a list in 'Num'.
lengthNum :: Num b => [a] -> b
lengthNum = fromIntegral . length

instance Encode File where
  encode File
    { header = h
    , segments = ss
    , threadS = ts
    } = encodeHeader ss (lengthNum bs) h ++ bs ++ tstate
    where
      bs :: [Word8]
      bs = foldl f [] ss

      f :: [Word8] -> Segment -> [Word8]
      f acc s = acc ++ encodeSegment dataOffset s

      -- | Indicates the offset in the file of the data.
      dataOffset :: Word64
      dataOffset = fromIntegral $ threadCommandSize + headerSize + sum (map g ss)

      -- | Gets the size of the segment load command plus the size of its sections.
      g :: Segment -> Word32
      g s = segmentSize + sectionSize * nsectsOf s

      tstate :: [Word8]
      tstate = encodeThreadState dataOffset ts

-- | The Mach-O header.
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

headerSize :: Word32
headerSize = 32

encodeHeader :: [Segment] -> Word32 -> Header -> [Word8]
encodeHeader ss l Header
  { magic      = m
  , cputype    = cpu
  , cpusubtype = cpusub
  , filetype   = f
  , hflags     = h
  } = mconcat
    [ concatMap encodeBits [m, cpu, cpusub]
    , encode f
    , concatMap encodeBits [ncmds, sizeofcmds, h, reserved]
    ]
  where
    ncmds :: Word32
    ncmds = lengthNum ss + 1 -- 1 is for LC_UNIXTHREAD

    sizeofcmds :: Word32
    sizeofcmds = l + threadCommandSize

    reserved :: Word32
    reserved = 0x00

data Segment = Segment
  { segname  :: String
  , maddr    :: Word64
  , msize    :: Word64
  , fsize    :: Word64
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
  , fsize    = 0
  , maxprot  = allProt
  , initprot = allProt
  , sections = []
  , segflags = 0
  }

pagezeroSize :: Word64
pagezeroSize = 0x100000000

pagezero :: Segment
pagezero = Segment
  { segname  = "__PAGEZERO"
  , maddr    = 0
  , msize    = pagezeroSize
  , fsize    = 0
  , maxprot  = []
  , initprot = []
  , sections = []
  , segflags = 0
  }

textSegment :: [Word8] -> Segment
textSegment text = Segment
  { segname  = "__TEXT"
  , maddr    = pagezeroSize
  , msize    = lengthNum text
  , fsize    = lengthNum text
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

encodeSegment :: Word64 -> Segment -> [Word8]
encodeSegment dataOffset Segment
  { segname  = n
  , maddr    = a
  , msize    = ms
  , fsize    = s
  , maxprot  = mp
  , initprot = ip
  , sections = ss
  , segflags = fs
  } = mconcat
    [ concatMap encodeBits [segment64, segmentSize + sectionSize * nsects]
    , encode n
    , concatMap encodeBits [a, dataoff ms, 0, dataoff s]
    , concatMap encode [mp, ip]
    , concatMap encodeBits [nsects, fs]
    , sects
    ]
  where
    nsects :: Word32
    nsects = lengthNum ss

    sects :: [Word8]
    sects = fst $ foldl f ([], dataOffset) ss

    f :: ([Word8], Word64) -> Section -> ([Word8], Word64)
    f (acc, off) s = (acc ++ encodeSection n off s, off + sizeOfSection s)

    -- FIXME: This is an ad hoc way.
    dataoff :: Word64 -> Word64
    dataoff x
      | s == 0    = x
      | otherwise = dataOffset + x

sizeOfSection :: Num a => Section -> a
sizeOfSection Section {size = s} = fromIntegral s

nsectsOf :: Num a => Segment -> a
nsectsOf Segment { sections = ss } = lengthNum ss

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
  , addr     = pagezeroSize
  , size     = lengthNum text
  , align    = 0
  , secflags = 0
  }

-- | The size of @section_64@.
sectionSize :: Word32
sectionSize = 80

encodeSection :: String -> Word64 -> Section -> [Word8]
encodeSection segn dataOffset Section
  { secname  = n
  , addr     = a
  , size     = s
  , align    = al
  , secflags = fs
  } = mconcat
    [ concatMap encode [n, segn]
    , concatMap encodeBits [a + dataOffset, s]
    , concatMap encodeBits $ [fromIntegral dataOffset, al, 0, 0, fs] ++ reserved
    ]
  where
    reserved :: [Word32]
    reserved = replicate 3 0x00

-- | 'Prot' represents a sort of virtual memory protection.
data Prot =
    Readable
  | Writable
  | Executable
  deriving Enum

instance Encode [Prot] where
  encode = encodeBits . foldr ((.|.) . pow2FromEnum) 0

pow2FromEnum :: Enum a => a -> Word32
pow2FromEnum e = 2 ^ fromEnum e

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

-- |
-- The Mach-O file type. It indicates the usage and alignment of the file.
-- It is encoded as a 'Word32', 4 bytes.
--
-- >>> encode Object
-- [1,0,0,0]
-- >>> encode Execute
-- [2,0,0,0]
data FileType =
    Object
  | Execute
  deriving Enum

instance Encode FileType where
  encode = encodeBits . pow2FromEnum

unixThread :: Word32
unixThread = 0x5

amd64ThreadState :: Word32
amd64ThreadState = 4

amd64ExceptionStateCount :: Word32
amd64ExceptionStateCount = 42

data ThreadState = ThreadState
  { rax    :: Word64
  , rbx    :: Word64
  , rcx    :: Word64
  , rdx    :: Word64
  , rdi    :: Word64
  , rsi    :: Word64
  , rbp    :: Word64
  , rsp    :: Word64
  , r8     :: Word64
  , r9     :: Word64
  , r10    :: Word64
  , r11    :: Word64
  , r12    :: Word64
  , r13    :: Word64
  , r14    :: Word64
  , r15    :: Word64
  -- , rip    :: Word64 -- Intended to be automatically set.
  , rflags :: Word64
  , cs     :: Word64
  , fs     :: Word64
  , gs     :: Word64
  }

threadCommandSize :: Word32
threadCommandSize = threadStateSize + 4*4

threadStateSize :: Word32
threadStateSize = 21 * 8

threadState :: ThreadState
threadState = ThreadState
  { rax    = 0
  , rbx    = 0
  , rcx    = 0
  , rdx    = 0
  , rdi    = 0
  , rsi    = 0
  , rbp    = 0
  , rsp    = 0
  , r8     = 0
  , r9     = 0
  , r10    = 0
  , r11    = 0
  , r12    = 0
  , r13    = 0
  , r14    = 0
  , r15    = 0
  , rflags = 0
  , cs     = 0
  , fs     = 0
  , gs     = 0
  }

encodeThreadState :: Word64 -> ThreadState -> [Word8]
encodeThreadState dataOffset ThreadState
  { rax    = x0
  , rbx    = x1
  , rcx    = x2
  , rdx    = x3
  , rdi    = x4
  , rsi    = x5
  , rbp    = x6
  , rsp    = x7
  , r8     = x8
  , r9     = x9
  , r10    = x10
  , r11    = x11
  , r12    = x12
  , r13    = x13
  , r14    = x14
  , r15    = x15
  , rflags = x16
  , cs     = x17
  , fs     = x18
  , gs     = x19
  } = concatMap encodeBits
    [ unixThread
    , threadCommandSize
    , amd64ThreadState
    , amd64ExceptionStateCount
    ] ++ concatMap encodeBits
    [ x0
    , x1
    , x2
    , x3
    , x4
    , x5
    , x6
    , x7
    , x8
    , x9
    , x10
    , x11
    , x12
    , x13
    , x14
    , x15
    , pagezeroSize + dataOffset
    , x16
    , x17
    , x18
    , x19
    ]
