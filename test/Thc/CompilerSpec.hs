module Thc.CompilerSpec where

import Test.Hspec

import qualified Data.ByteString.Lazy as B

import Thc.Code
import Thc.Compiler
import Thc.Expr

spec :: Spec
spec = do
  describe "compile" $
    it "compiles a Term to an executable binary file" $ do
      want <- B.readFile "test/Thc/data/exit1"
      compile (Var "a") Darwin Amd64 `shouldBe` Right want
