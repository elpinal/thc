module Thc.Expr.IndexedSpec where

import Test.Hspec

import qualified Thc.Expr as E
import Thc.Expr.Indexed

spec :: Spec
spec = do
  describe "fromNamed" $ do
    it "converts a named term to unnamed one" $ do
      let x = E.Abs "f" $
                E.App
                  (E.Var "f") $
                  E.Abs "g" $
                    E.App
                      (E.Var "g") $
                      E.Lit (Int 44)

      let y = Abs "f" $
                App
                  (Var "f" 0 1) $
                  Abs "g" $
                    App
                      (Var "g" 0 2) $
                      Lit (Int 44)

      fromNamed x                                   `shouldBe` return y
      fromNamed (E.Abs "x" $ E.Var "x")             `shouldBe` return (Abs "x" $ Var "x" 0 1)
      fromNamed (E.Abs "a" $ E.Abs "b" $ E.Var "a") `shouldBe` return (Abs "a" $ Abs "b" $ Var "a" 1 2)

    context "when given unbound idendifiers" $
      it "returns Nothing" $ do
        fromNamed (E.Var "x") `shouldBe` Nothing
