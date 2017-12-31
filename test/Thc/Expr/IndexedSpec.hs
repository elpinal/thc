module Thc.Expr.IndexedSpec where

import Test.Hspec

import qualified Thc.Expr as E
import Thc.Expr.Indexed
import qualified Thc.Type as T

tuplePat = E.PTuple . map E.PVar

spec :: Spec
spec = do
  describe "fromNamed" $ do
    it "converts a named term to unnamed one" $ do
      let x = E.Abs (E.PVar "f") T.Bool $
                E.App
                  (E.Var "f") $
                  E.Abs (E.PVar "g") T.Bool $
                    E.App
                      (E.Var "g") $
                      E.Lit (Int 44)

      let y = Abs (E.PVar "f") T.Bool $
                App
                  (Var "f" 0 1) $
                  Abs (E.PVar "g") T.Bool $
                    App
                      (Var "g" 0 2) $
                      Lit (Int 44)

      fromNamed x                                                                  `shouldBe` return y
      fromNamed (E.Abs (E.PVar "x") T.Bool $ E.Var "x")                            `shouldBe` return (Abs (E.PVar "x") T.Bool $ Var "x" 0 1)
      fromNamed (E.Abs (E.PVar "a") T.Int $ E.Abs (E.PVar "b") T.Bool $ E.Var "a") `shouldBe` return (Abs (E.PVar "a") T.Int $ Abs (E.PVar "b") T.Bool $ Var "a" 1 2)

    context "when given duplicated variables in a tuple pattern" $
      it "returns Nothing" $ do
        fromNamed (E.Abs (tuplePat ["a", "b"]) (T.Tuple [T.Int, T.Int]) $ E.Lit $ Int 0) `shouldBe` return (Abs (tuplePat ["a", "b"]) (T.Tuple [T.Int, T.Int]) $ Lit $ Int 0)
        fromNamed (E.Abs (tuplePat ["a", "a"]) (T.Tuple [T.Int, T.Int]) $ E.Lit $ Int 0) `shouldBe` Nothing

    context "when given unbound idendifiers" $
      it "returns Nothing" $ do
        fromNamed (E.Var "x") `shouldBe` Nothing

  describe "typeOf" $ do
    context "when given a typable term" $ do
      it "gets the type of the term" $ do
        typeOf (Lit $ Bool True)                                                                                 `shouldBe` return T.Bool
        typeOf (Abs (E.PVar "x") T.Int $ Var "x" 0 1)                                                            `shouldBe` return (T.Int T.:->: T.Int)
        typeOf (Abs (E.PVar "f") (T.Int T.:->: T.Bool) $ Abs (E.PVar "x") T.Bool $ Var "f" 0 1)                  `shouldBe` return ((T.Int T.:->: T.Bool) T.:->: T.Bool T.:->: T.Bool)
        typeOf (Abs (E.PVar "f") (T.Int T.:->: T.Bool) $ Abs (E.PVar "x") T.Int $ Var "f" 1 2 `App` Var "x" 0 2) `shouldBe` return ((T.Int T.:->: T.Bool) T.:->: T.Int T.:->: T.Bool)

    context "when given a non-typable term" $ do
      it "returns Nothing" $ do
        typeOf (Abs (E.PVar "f") (T.Int T.:->: T.Int) $ Abs (E.PVar "x") T.Bool $ Var "f" 1 2 `App` Var "x" 0 2) `shouldBe` Nothing
        typeOf (Abs (E.PVar "x") T.Int $ Var "x" 0 1 `App` Var "x" 0 1)                                          `shouldBe` Nothing

  describe "eval" $ do
    context "when given a tuple" $ do
      it "evaluates terms in tuples" $ do
        eval (Tuple [Lit $ Int 0])                                               `shouldBe` Tuple [Lit $ Int 0]
        eval (Tuple [Abs (E.PVar "x") T.Int $ Var "x" 0 1])                      `shouldBe` Tuple [Abs (E.PVar "x") T.Int $ Var "x" 0 1]
        eval (Tuple [App (Abs (E.PVar "x") T.Int $ Var "x" 0 1) (Lit $ Int 12)]) `shouldBe` Tuple [Lit $ Int 12]

    context "when given a tuple as the pattern in a lambda abstraction" $ do
      it "evaluates it binding each variable to a item" $ do
        let swap = Abs (tuplePat ["a", "b"]) (T.Tuple [T.Int, T.Int]) $ Tuple [Var "b" 1 2, Var "a" 0 2]
        eval (App swap $ Tuple [Lit $ Int 0, Lit $ Int 128]) `shouldBe` Tuple [Lit $ Int 128, Lit $ Int 0]

        let idTuple = Abs (E.PVar "abc") (T.Tuple [T.Int, T.Int]) $ Var "abc" 0 1
        eval (App swap $ App idTuple $ Tuple [Lit $ Int 0, Lit $ Int 128]) `shouldBe` Tuple [Lit $ Int 128, Lit $ Int 0]

  describe "evalForPat" $ do
    it "evaluates a term for a pattern" $ do
      evalForPat (E.PVar "x") (Lit $ Int 1) `shouldBe` return (Lit $ Int 1)
