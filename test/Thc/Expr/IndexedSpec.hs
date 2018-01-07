module Thc.Expr.IndexedSpec where

import Test.Hspec

import Data.Either
import qualified Data.Map.Lazy as Map
import Data.Semigroup

import qualified Thc.Expr as E
import Thc.Expr.Indexed
import qualified Thc.Type as T

tuplePat = E.tuplePat

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
                      int 44

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

      let p = PTuple [tuplePat ["a", "b"], tuplePat ["c", "d"]]
          ty = (T.Tuple [T.Tuple [T.Int, T.Int], T.Tuple [T.Int, T.Int]])
      fromNamed (E.Abs p ty $ E.Var "b") `shouldBe` return (Abs p ty $ Var "b" 2 4)

      fromNamed (E.Record [])                                    `shouldBe` return (Record [])
      fromNamed (E.Record [("", int 1)])                         `shouldBe` return (Record [("", int 1)])
      fromNamed (E.Record [("x", E.Abs (PVar "_") T.Unit unit)]) `shouldBe` return (Record [("x", Abs (PVar "_") T.Unit $ Lit Unit)])

      fromNamed (E.Ann (int 78) T.Int) `shouldBe` return (Ann (int 78) T.Int)

    context "when given duplicated variables in a tuple pattern" $
      it "returns an error" $ do
        fromNamed (E.Abs (tuplePat ["a", "b"]) (T.Tuple [T.Int, T.Int]) $ int 0) `shouldBe` return (Abs (tuplePat ["a", "b"]) (T.Tuple [T.Int, T.Int]) $ Lit $ Int 0)
        fromNamed (E.Abs (tuplePat ["a", "a"]) (T.Tuple [T.Int, T.Int]) $ int 0) `shouldSatisfy` isLeft

    context "when given unbound idendifiers" $
      it "returns an error" $ do
        fromNamed (E.Var "x") `shouldSatisfy` isLeft

  describe "typeOf" $ do
    context "when given a typable term" $ do
      it "gets the type of the term" $ do
        typeOf (bool True)                                                                                   `shouldNotThrow` return T.Bool
        typeOf (Abs (PVar "x") T.Int $ Var "x" 0 1)                                                          `shouldNotThrow` return (T.Int T.:->: T.Int)
        typeOf (Abs (PVar "f") (T.Int T.:->: T.Bool) $ Abs (PVar "x") T.Bool $ Var "f" 0 1)                  `shouldNotThrow` return ((T.Int T.:->: T.Bool) T.:->: T.Bool T.:->: T.Bool)
        typeOf (Abs (PVar "f") (T.Int T.:->: T.Bool) $ Abs (PVar "x") T.Int $ Var "f" 1 2 `App` Var "x" 0 2) `shouldNotThrow` return ((T.Int T.:->: T.Bool) T.:->: T.Int T.:->: T.Bool)

        typeOf (Record [])                   `shouldNotThrow` return (T.Record [])
        typeOf (Record [("a", Lit $ Int 1)]) `shouldNotThrow` return (T.Record [("a", T.Int)])

    context "when given a Case" $ do
      let t = Tuple [int 3, bool False]
          p = tuplePat ["x", "y"]

      it "returns the type which is all the arms identically have" $ do
        typeOf (Case (bool True) $ return (PVar "x", int 12))      `shouldNotThrow` return T.Int
        typeOf (Case (bool True) $ return (PVar "x", Var "x" 0 1)) `shouldNotThrow` return T.Bool

        typeOf (Case t $ return (p, Var "x" 1 2)) `shouldNotThrow` return T.Int
        typeOf (Case t $ return (p, Var "y" 0 2)) `shouldNotThrow` return T.Bool

        let t = Tagged "a" $ int 88
            p = PVar "x"
            ty = T.variant [("a", T.Int)]
        typeOf (Case t $ return (p, Var "x" 0 1))          `shouldNotThrow` Left (BareVariant "a" $ int 88)
        typeOf (Case (Ann t ty) $ return (p, Var "x" 0 1)) `shouldNotThrow` return ty

        let p = PVariant "a" $ PVar "x"
            q = PVariant "nolabel" $ PVar "x"
            s = Var "x" 0 1
            a = return (p, s)
            b = return (q, s)
        typeOf (Case (Ann t ty) a)        `shouldNotThrow` return T.Int
        typeOf (Case (Ann t ty) $ a <> a) `shouldNotThrow` return T.Int
        typeOf (Case (Ann t ty) b)        `shouldNotThrow` Left (EvalError $ PatternMismatch q ty)
        typeOf (Case (Ann t ty) $ a <> b) `shouldNotThrow` Left (EvalError $ PatternMismatch q ty)
        typeOf (Case (Ann t ty) $ b <> a) `shouldNotThrow` Left (EvalError $ PatternMismatch q ty)

      context "when the patterns are inconsistent" $ do
        it "returns an error" $ do
          let p1 = tuplePat ["x", "y", "z"]
          typeOf (Case t $ return (p, Var "y" 0 2) <> return (p1, Var "z" 0 3)) `shouldNotThrow` Left (EvalError $ PatternMismatch p1 $ T.Tuple [T.Int, T.Bool])

          let p = tuplePat ["x"]
          typeOf (Case (bool True) $ return (p, Var "x" 0 1)) `shouldNotThrow` Left (EvalError $ PatternMismatch p T.Bool)

    context "when given a tagged term to which is not annotated its type" $ do
      it "returns an error" $ do
        typeOf (Tagged "l" $ int 0) `shouldNotThrow` Left (BareVariant "l" $ int 0)

    context "when given a tagged term with an annotation" $ do
      it "returns the annotated type after verifying the term" $ do
        typeOf (Ann (Tagged "l" $ int 0) $ T.variant []) `shouldNotThrow` Left (VariantError "l" (int 0) Map.empty)

        let ty = T.variant [("l", T.Int)]
        typeOf (Ann (Tagged "l" $ int 0) ty) `shouldNotThrow` return ty

        let ts = Map.singleton "aaa" T.Int
        typeOf (Ann (Tagged "l" $ int 0) $ T.Variant ts) `shouldNotThrow` Left (VariantError "l" (int 0) ts)

        let ts = Map.singleton "l" T.Bool
        typeOf (Ann (Tagged "l" $ int 0) $ T.Variant ts) `shouldNotThrow` Left (TypeMismatch T.Int T.Bool)

        let ty = T.variant [("l", T.Int), ("x", T.Unit)]
        typeOf (Ann (Tagged "l" $ int 0) ty) `shouldNotThrow` return ty

    context "when given annotations" $ do
      it "tests that the type of a term is equal to the annotated type" $ do
        let l = Lit $ Bool False
        typeOf (Ann l T.Bool) `shouldNotThrow` return T.Bool
        typeOf (Ann l T.Int)  `shouldNotThrow` Left (TypeMismatch T.Bool T.Int)

    context "when given a non-typable term" $ do
      it "returns an error" $ do
        typeOf (Abs (PVar "f") (T.Int T.:->: T.Int) $ Abs (PVar "x") T.Bool $ Var "f" 1 2 `App` Var "x" 0 2) `shouldNotThrow` Left (IllTypedApp (Var "f" 1 2) T.Bool)
        typeOf (Abs (PVar "x") T.Int $ Var "x" 0 1 `App` Var "x" 0 1)                                        `shouldNotThrow` Left (IllTypedApp (Var "x" 0 1) T.Int)

  describe "eval" $ do
    context "when given a tuple" $ do
      it "evaluates terms in tuples" $ do
        eval (Tuple [Lit $ Int 0])                                             `shouldBe` Tuple [Lit $ Int 0]
        eval (Tuple [Abs (PVar "x") T.Int $ Var "x" 0 1])                      `shouldBe` Tuple [Abs (PVar "x") T.Int $ Var "x" 0 1]
        eval (Tuple [App (Abs (PVar "x") T.Int $ Var "x" 0 1) (Lit $ Int 12)]) `shouldBe` Tuple [Lit $ Int 12]

    context "when given a tuple as the pattern in a lambda abstraction" $ do
      it "evaluates it binding each variable to a item" $ do
        let swap = Abs (tuplePat ["a", "b"]) (T.Tuple [T.Int, T.Int]) $ Tuple [Var "b" 0 2, Var "a" 1 2]
        eval (App swap $ Tuple [Lit $ Int 0, Lit $ Int 128]) `shouldBe` Tuple [Lit $ Int 128, Lit $ Int 0]

        let idTuple = Abs (PVar "abc") (T.Tuple [T.Int, T.Int]) $ Var "abc" 0 1
        eval (App idTuple $ Tuple [Lit $ Int 0, Lit $ Int 128])            `shouldBe` Tuple [Lit $ Int 0, Lit $ Int 128]
        eval (App swap $ App idTuple $ Tuple [Lit $ Int 0, Lit $ Int 128]) `shouldBe` Tuple [Lit $ Int 128, Lit $ Int 0]

        let fstsnd = Abs (PTuple [tuplePat ["a", "b"], tuplePat ["c", "d"]]) (T.Tuple [T.Tuple [T.Int, T.Int], T.Tuple [T.Int, T.Int]]) $ Var "b" 2 4
        eval (App fstsnd $ Tuple [Tuple [Lit $ Int 8, Lit $ Int 16], Tuple [Lit $ Int 32, Lit $ Int 64]]) `shouldBe` Lit (Int 16)

    context "when given annotated values" $ do
      it "removes the annotations" $ do
        eval (Ann (int 12) T.Int) `shouldBe` int 12
        -- Assumes a term well typed. Even if the annotation is wrong, it is ignored.
        eval (Ann (int 12) T.Bool) `shouldBe` int 12

        let t = Abs (PVar "x") T.Bool $ int 0
        eval (Ann t $ T.Bool T.:->: T.Int) `shouldBe` t

    context "when given a case-expression" $ do
      it "select one arm whose pattern is appropriate to a term; patterns are tested from up to down" $ do
        let t = int 7
            a = return (PVar "k", Var "k" 0 1)
            b = return (PVar "l", Var "l" 0 1)
        eval (Case t $ a)           `shouldBe` t
        eval (Case t $ a <> a)      `shouldBe` t
        eval (Case t $ b)           `shouldBe` t
        eval (Case t $ b <> b)      `shouldBe` t
        eval (Case t $ a <> b)      `shouldBe` t
        eval (Case t $ b <> a)      `shouldBe` t
        eval (Case t $ a <> a <> a) `shouldBe` t

        let t1 = bool True
        eval (Case t $ return (PVar "k", t1)) `shouldBe` t1

        let tuple = Tuple [t, t1]
            p     = tuplePat ["x", "y"]
            c     = return (p, Var "x" 1 2)
            d     = return (p, Var "y" 0 2)
        eval (Case tuple $ a) `shouldBe` tuple
        eval (Case tuple $ c) `shouldBe` t
        eval (Case tuple $ d) `shouldBe` t1

  describe "reduce" $ do
    it "do beta-reduction" $ do
      let t = Lit $ Bool True
      reduce (PVar "x") (Lit $ Int 8) t `shouldNotThrow` t

      reduce (PVar "x") (Lit $ Int 12) (Var "y" 1 2) `shouldNotThrow` Var "y" 0 1
      let t = Var "x" 0 1
      reduce (PVar "x") (Var "y" 1 2) t `shouldNotThrow` Var "y" 1 2

    context "when a pattern and an argument are mismatched" $ do
      it "throws an exception" $ do
        -- TODO:
        -- In error handling, trying to get the type of the term, throws the WrongIndex exception if it's unbound.
        -- But in the time, for more detailed error messages, the Context may be needed.
        reduce (tuplePat ["x"]) (Var "y" 1 2) (Var "x" 0 1) `shouldThrow` anyException

  describe "evalForPat" $ do
    it "evaluates a term for a pattern" $ do
      evalForPat (PVar "x") (Lit $ Int 1)                                                   `shouldNotThrow` Lit (Int 1)
      evalForPat (tuplePat ["a", "b", "c"]) (Tuple [])                                      `shouldNotThrow` Tuple []
      evalForPat (tuplePat ["a", "b", "c"]) (Tuple [Var "a" 0 1])                           `shouldNotThrow` Tuple [Var "a" 0 1]
      evalForPat (tuplePat ["a", "b", "c"]) (Tuple [Var "a" 0 1, Var "a" 0 1, Var "a" 0 1]) `shouldNotThrow` Tuple [Var "a" 0 1, Var "a" 0 1, Var "a" 0 1]
      let idTuple = Abs (PVar "t") (T.Tuple [T.Int, T.Int, T.Int]) $ Var "t" 0 1
          int12 = Lit $ Int 12
      evalForPat (tuplePat ["a", "b", "c"]) (idTuple `App` Tuple (replicate 3 int12)) `shouldNotThrow` Tuple (replicate 3 int12)

      let tuple = Tuple $ map Tuple [map int [12, 28], map int [39, 54]]
          p = PTuple [tuplePat ["c", "d"], tuplePat ["e", "f"]]
      evalForPat p tuple                                      `shouldNotThrow` tuple
      evalForPat p (Abs (PVar "x") (T.Int) tuple `App` int 2) `shouldNotThrow` tuple

    context "when the type check has not been done" $ do
      it "throws an exception" $ do
        evalForPat (tuplePat ["a", "b", "c"]) (Lit $ Int 1) `shouldThrow` anyException
