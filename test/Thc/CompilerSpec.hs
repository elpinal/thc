module Thc.CompilerSpec where

import Test.Hspec

import qualified Data.ByteString.Lazy as B

import qualified Thc.Code as Code
import Thc.Compiler
import Thc.Expr

spec :: Spec
spec = do
  describe "compile" $ do
    it "compiles a Term to an executable binary file" $ do
      want <- B.readFile "test/Thc/data/exit178"
      compile (Lit $ Int 178) Code.Darwin Code.Amd64 `shouldBe` Right want

      want <- B.readFile "test/Thc/data/exit81"
      compile (Lit $ Bool True) Code.Darwin Code.Amd64 `shouldBe` Right want

    context "when given an invalid program" $
      it "returns an error" $ do
        compile (Var "x") Code.Darwin Code.Amd64 `shouldBe` Left Unbound
