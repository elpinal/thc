module Thc.TypeSpec where

import Test.Hspec

import Thc.Type

spec :: Spec
spec = do
  describe "subtype" $
    it "tests subtyping" $ do
      subtype Int Int  `shouldBe` True
      subtype Int Bool `shouldBe` False
