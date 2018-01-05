module Thc.TypeSpec where

import Test.Hspec

import Thc.Type

spec :: Spec
spec = do
  describe "subtype" $
    it "tests subtyping" $ do
      subtype Int Int  `shouldBe` True
      subtype Int Bool `shouldBe` False

      let nint = variant [("n", Int)]
      subtype Int nint          `shouldBe` False
      subtype nint nint         `shouldBe` True
      subtype (variant []) nint `shouldBe` True

      subtype (variant [("x", Int)]) nint  `shouldBe` False
      subtype (variant [("n", Bool)]) nint `shouldBe` False
