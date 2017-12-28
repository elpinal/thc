module Thc.AsmSpec where

import Test.Hspec

import Thc.Asm
import qualified Thc.Tac as Tac

spec :: Spec
spec = do
  describe "fromTac" $
    it "translates Tac to Asm" $ do
      let r = Tac.Return $ Int 23
      fromTac' r `shouldBe` r
