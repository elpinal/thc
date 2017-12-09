module Thc.Code where

import qualified Data.ByteString.Lazy as B
import Data.Word

import Thc.Asm

type Code = B.ByteString

class Machine m where
  fromAsm :: Asm -> m Code
