module OS.Darwin where

import Data.Bits

import qualified Data.ByteString.Lazy as B

import FileFormat.Macho
import Thc.Code

updateContext :: Context -> Context
updateContext ctx = ctx {os = f}
  where
    f c o bs
      | o == Darwin && c == Amd64 = return . B.pack . executableFromText $ B.unpack bs
      | otherwise                 = os ctx c o bs

syscallClassShift = 24

syscallClassMask = shift 0xff syscallClassShift

syscallNumberMask = complement syscallClassMask

data SyscallClass =
    None -- ^ Invalid
  | Mach -- ^ Mach
  | Unix -- ^ Unix/Bsd
  | Mdep -- ^ Machine-dependent
  | Diag -- ^ Diagnostics
  | Ipc  -- ^ Mach IPC
  deriving Enum

syscallConstructUnix n = shift (fromEnum Unix) syscallClassShift .|. syscallNumberMask.&.n

syscallExit = 1
