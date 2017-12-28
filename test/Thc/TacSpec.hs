module Thc.TacSpec where

import Test.Hspec

import Thc.Expr.Indexed
import Thc.Tac

spec :: Spec
spec = do
  describe "fromLit" $
    it "translates Expr to Tac" $ do
      fromLit (Int 12)     `shouldBe` Return (Int 12)
      fromLit (Bool False) `shouldBe` Return (Bool False)
