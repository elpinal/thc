module Thc.Expr.IndexedSpec where

import Test.Hspec

import Data.Either

import qualified Thc.Expr as E
import Thc.Expr.Indexed
import qualified Thc.Type as T

tuplePat = PTuple . map PVar

shouldNotThrow :: (HasCallStack, Eq a, Show a) => IO a -> a -> Expectation
shouldNotThrow x y = do
  x' <- x
  x' `shouldBe` y

spec :: Spec
spec = do
  describe "fromNamed" $ do
    it "converts a named term to unnamed one" $ do
      let x = E.Abs (PVar "f") T.Bool $
                E.App
                  (E.Var "f") $
                  E.Abs (PVar "g") T.Bool $
                    E.App
                      (E.Var "g") $
                      E.Lit (Int 44)

      let y = Abs (PVar "f") T.Bool $
                App
                  (Var "f" 0 1) $
                  Abs (PVar "g") T.Bool $
                    App
                      (Var "g" 0 2) $
                      Lit (Int 44)

      fromNamed x                                                              `shouldBe` return y
      fromNamed (E.Abs (PVar "x") T.Bool $ E.Var "x")                          `shouldBe` return (Abs (PVar "x") T.Bool $ Var "x" 0 1)
      fromNamed (E.Abs (PVar "a") T.Int $ E.Abs (PVar "b") T.Bool $ E.Var "a") `shouldBe` return (Abs (PVar "a") T.Int $ Abs (PVar "b") T.Bool $ Var "a" 1 2)

    context "when given duplicated variables in a tuple pattern" $
      it "returns an error" $ do
        fromNamed (E.Abs (tuplePat ["a", "b"]) (T.Tuple [T.Int, T.Int]) $ E.Lit $ Int 0) `shouldBe` return (Abs (tuplePat ["a", "b"]) (T.Tuple [T.Int, T.Int]) $ Lit $ Int 0)
        fromNamed (E.Abs (tuplePat ["a", "a"]) (T.Tuple [T.Int, T.Int]) $ E.Lit $ Int 0) `shouldSatisfy` isLeft

    context "when given unbound idendifiers" $
      it "returns an error" $ do
        fromNamed (E.Var "x") `shouldSatisfy` isLeft

  describe "typeOf" $ do
    context "when given a typable term" $ do
      it "gets the type of the term" $ do
        typeOf (Lit $ Bool True)                                                                             `shouldBe` return T.Bool
        typeOf (Abs (PVar "x") T.Int $ Var "x" 0 1)                                                          `shouldBe` return (T.Int T.:->: T.Int)
        typeOf (Abs (PVar "f") (T.Int T.:->: T.Bool) $ Abs (PVar "x") T.Bool $ Var "f" 0 1)                  `shouldBe` return ((T.Int T.:->: T.Bool) T.:->: T.Bool T.:->: T.Bool)
        typeOf (Abs (PVar "f") (T.Int T.:->: T.Bool) $ Abs (PVar "x") T.Int $ Var "f" 1 2 `App` Var "x" 0 2) `shouldBe` return ((T.Int T.:->: T.Bool) T.:->: T.Int T.:->: T.Bool)

    context "when given a non-typable term" $ do
      it "returns Nothing" $ do
        typeOf (Abs (PVar "f") (T.Int T.:->: T.Int) $ Abs (PVar "x") T.Bool $ Var "f" 1 2 `App` Var "x" 0 2) `shouldBe` Nothing
        typeOf (Abs (PVar "x") T.Int $ Var "x" 0 1 `App` Var "x" 0 1)                                        `shouldBe` Nothing

  describe "eval" $ do
    context "when given a tuple" $ do
      it "evaluates terms in tuples" $ do
        eval (Tuple [Lit $ Int 0])                                             `shouldBe` Tuple [Lit $ Int 0]
        eval (Tuple [Abs (PVar "x") T.Int $ Var "x" 0 1])                      `shouldBe` Tuple [Abs (PVar "x") T.Int $ Var "x" 0 1]
        eval (Tuple [App (Abs (PVar "x") T.Int $ Var "x" 0 1) (Lit $ Int 12)]) `shouldBe` Tuple [Lit $ Int 12]

    context "when given a tuple as the pattern in a lambda abstraction" $ do
      it "evaluates it binding each variable to a item" $ do
        let swap = Abs (tuplePat ["a", "b"]) (T.Tuple [T.Int, T.Int]) $ Tuple [Var "b" 1 2, Var "a" 0 2]
        eval (App swap $ Tuple [Lit $ Int 0, Lit $ Int 128]) `shouldBe` Tuple [Lit $ Int 128, Lit $ Int 0]

        let idTuple = Abs (PVar "abc") (T.Tuple [T.Int, T.Int]) $ Var "abc" 0 1
        eval (App swap $ App idTuple $ Tuple [Lit $ Int 0, Lit $ Int 128]) `shouldBe` Tuple [Lit $ Int 128, Lit $ Int 0]

  describe "evalForPat" $ do
    it "evaluates a term for a pattern" $ do
      evalForPat (PVar "x") (Lit $ Int 1)                                                   `shouldNotThrow` Lit (Int 1)
      evalForPat (tuplePat ["a", "b", "c"]) (Tuple [])                                      `shouldNotThrow` Tuple []
      evalForPat (tuplePat ["a", "b", "c"]) (Tuple [Var "a" 0 1])                           `shouldNotThrow` Tuple [Var "a" 0 1]
      evalForPat (tuplePat ["a", "b", "c"]) (Tuple [Var "a" 0 1, Var "a" 0 1, Var "a" 0 1]) `shouldNotThrow` Tuple [Var "a" 0 1, Var "a" 0 1, Var "a" 0 1]
      let idTuple = Abs (PVar "t") (T.Tuple [T.Int, T.Int, T.Int]) $ Var "t" 0 1
      evalForPat (tuplePat ["a", "b", "c"]) (idTuple `App` Tuple [Var "a" 0 1, Var "a" 0 1, Var "a" 0 1]) `shouldNotThrow` Tuple [Var "a" 0 1, Var "a" 0 1, Var "a" 0 1]

    context "when the type check has not been done" $ do
      it "throws an exception" $ do
        evalForPat (tuplePat ["a", "b", "c"]) (Lit $ Int 1) `shouldThrow` anyException
