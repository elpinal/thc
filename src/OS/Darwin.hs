module OS.Darwin where

import Data.Bits

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
