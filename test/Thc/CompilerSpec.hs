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
      want <- B.readFile "test/Thc/data/exit178"
      compile (Lit $ Int 178) Darwin Amd64 `shouldBe` Right want

      want <- B.readFile "test/Thc/data/exit81"
      compile (Lit $ Bool True) Darwin Amd64 `shouldBe` Right want
