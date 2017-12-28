module Thc.Expr.IndexedSpec where

import Test.Hspec

import qualified Thc.Expr as E
import Thc.Expr.Indexed

spec :: Spec
spec = do
  describe "fromNamed" $ do
    it "converts a named term to unnamed one" $ do
      fromNamed (E.Abs "x" $ E.Var "x") `shouldBe` return (Abs "x" $ Var "x" 0 1)

    context "when given unbound idendifiers" $
      it "returns Nothing" $ do
        fromNamed (E.Var "x") `shouldBe` Nothing
