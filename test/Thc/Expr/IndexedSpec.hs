module Thc.Expr.IndexedSpec where

import Test.Hspec

import qualified Thc.Expr as E
import Thc.Expr.Indexed
import qualified Thc.Type as T

spec :: Spec
spec = do
  describe "fromNamed" $ do
    it "converts a named term to unnamed one" $ do
      let x = E.Abs "f" T.Bool $
                E.App
                  (E.Var "f") $
                  E.Abs "g" T.Bool $
                    E.App
                      (E.Var "g") $
                      E.Lit (Int 44)

      let y = Abs "f" T.Bool $
                App
                  (Var "f" 0 1) $
                  Abs "g" T.Bool $
                    App
                      (Var "g" 0 2) $
                      Lit (Int 44)

      fromNamed x                                                `shouldBe` return y
      fromNamed (E.Abs "x" T.Bool $ E.Var "x")                   `shouldBe` return (Abs "x" T.Bool $ Var "x" 0 1)
      fromNamed (E.Abs "a" T.Int $ E.Abs "b" T.Bool $ E.Var "a") `shouldBe` return (Abs "a" T.Int $ Abs "b" T.Bool $ Var "a" 1 2)

    context "when given unbound idendifiers" $
      it "returns Nothing" $ do
        fromNamed (E.Var "x") `shouldBe` Nothing
