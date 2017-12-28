module OS.Darwin
  ( updateContext
  ) where

import Data.Bits

import FileFormat.Macho
import Thc.Code

updateContext :: Context -> Context
updateContext ctx = ctx {os = f}
  where
    f c o
      | o == Darwin && c == Amd64 = return . executableFromText
      | otherwise                 = os ctx c o

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
