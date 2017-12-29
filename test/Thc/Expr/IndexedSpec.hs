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

  describe "typeOf" $ do
    context "when given a typable term" $ do
      it "gets the type of the term" $ do
        typeOf (Lit $ Bool True)                                                               `shouldBe` return T.Bool
        typeOf (Abs "x" T.Int $ Var "x" 0 1)                                                   `shouldBe` return (T.Int T.:->: T.Int)
        typeOf (Abs "f" (T.Int T.:->: T.Bool) $ Abs "x" T.Bool $ Var "f" 0 1)                  `shouldBe` return ((T.Int T.:->: T.Bool) T.:->: (T.Bool T.:->: T.Bool))
        typeOf (Abs "f" (T.Int T.:->: T.Bool) $ Abs "x" T.Int $ Var "f" 1 2 `App` Var "x" 0 2) `shouldBe` return ((T.Int T.:->: T.Bool) T.:->: (T.Int T.:->: T.Bool))

    context "when given a non-typable term" $ do
      it "returns Nothing" $ do
        typeOf (Abs "f" (T.Int T.:->: T.Int) $ Abs "x" T.Bool $ Var "f" 1 2 `App` Var "x" 0 2) `shouldBe` Nothing
        typeOf (Abs "x" T.Int $ Var "x" 0 1 `App` Var "x" 0 1)                                 `shouldBe` Nothing
