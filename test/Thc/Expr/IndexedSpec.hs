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

shouldNotThrowM :: (HasCallStack, Eq (m a), Show (m a), Monad m) => IO (m a) -> a -> Expectation
shouldNotThrowM x y = do
  x' <- x
  x' `shouldBe` return y

spec :: Spec
spec = do
  describe "fromNamed" $ do
    it "converts a named term to unnamed one" $ do
      let x = E.abst (PVar "f") T.Bool $
                E.App
                  (E.Var "f") $
                  E.abst (PVar "g") T.Bool $
                    E.App
                      (E.Var "g") $
                      int 44

      let y = abst (PVar "f") T.Bool $
                App
                  (Var "f" 0 1) $
                  abst (PVar "g") T.Bool $
                    App
                      (Var "g" 0 2) $
                      Lit (Int 44)

      fromNamed x                                                                `shouldBe` return y
      fromNamed (E.abst (PVar "x") T.Bool $ E.Var "x")                           `shouldBe` return (abst (PVar "x") T.Bool $ Var "x" 0 1)
      fromNamed (E.abst (PVar "a") T.Int $ E.abst (PVar "b") T.Bool $ E.Var "a") `shouldBe` return (abst (PVar "a") T.Int $ abst (PVar "b") T.Bool $ Var "a" 1 2)

      let p = PTuple [tuplePat ["a", "b"], tuplePat ["c", "d"]]
          ty = (T.Tuple [T.Tuple [T.Int, T.Int], T.Tuple [T.Int, T.Int]])
      fromNamed (E.abst p ty $ E.Var "b") `shouldBe` return (abst p ty $ Var "b" 2 4)

      fromNamed (E.Record [])                                     `shouldBe` return (Record [])
      fromNamed (E.Record [("", int 1)])                          `shouldBe` return (Record [("", int 1)])
      fromNamed (E.Record [("x", E.abst (PVar "_") T.Unit unit)]) `shouldBe` return (Record [("x", abst (PVar "_") T.Unit $ Lit Unit)])

      let t1 = int 78
          t2 = int 78
      fromNamed (E.Ann t1 T.Int) `shouldBe` return (Ann t2 T.Int)

      let a = return (PVar "x", int 12)
          b = return (PVar "x", int 12)
      fromNamed (E.Case t1 a) `shouldBe` return (Case t2 b)

    context "when given duplicated variables in a tuple pattern" $
      it "returns an error" $ do
        fromNamed (E.abst (tuplePat ["a", "b"]) (T.Tuple [T.Int, T.Int]) $ int 0) `shouldBe` return (abst (tuplePat ["a", "b"]) (T.Tuple [T.Int, T.Int]) $ Lit $ Int 0)
        fromNamed (E.abst (tuplePat ["a", "a"]) (T.Tuple [T.Int, T.Int]) $ int 0) `shouldSatisfy` isLeft

    context "when given unbound idendifiers" $
      it "returns an error" $ do
        fromNamed (E.Var "x") `shouldSatisfy` isLeft

    context "when given a lambda abstraction whose parameter does not match the annotated type" $
      it "does not check the typing error" $ do
        fromNamed (E.abst (tuplePat ["nn"]) T.Int $ E.Var "nn") `shouldBe` return (abst (tuplePat ["nn"]) T.Int $ Var "nn" 0 1)

  describe "principal" $ do
    context "when given a typable term" $ do
      it "gets the type of the term" $ do
        principal (bool True)                                                                                     `shouldNotThrowM` T.Bool
        principal (abst (PVar "x") T.Int $ Var "x" 0 1)                                                           `shouldNotThrowM` (T.Int T.:->: T.Int)
        principal (abst (PVar "f") (T.Int T.:->: T.Bool) $ abst (PVar "x") T.Bool $ Var "f" 0 1)                  `shouldNotThrowM` ((T.Int T.:->: T.Bool) T.:->: T.Bool T.:->: T.Bool)
        principal (abst (PVar "f") (T.Int T.:->: T.Bool) $ abst (PVar "x") T.Int $ Var "f" 1 2 `App` Var "x" 0 2) `shouldNotThrowM` ((T.Int T.:->: T.Bool) T.:->: T.Int T.:->: T.Bool)

        principal (Record [])                   `shouldNotThrowM` (T.Record [])
        principal (Record [("a", Lit $ Int 1)]) `shouldNotThrowM` (T.Record [("a", T.Int)])

        principal (abst (int 1) T.Int unit)             `shouldNotThrowM` (T.Int T.:->: T.Unit)
        principal (abst (int 1) T.Int unit `App` int 1) `shouldNotThrowM` T.Unit
        principal (abst (int 1) T.Int unit `App` int 2) `shouldNotThrowM` T.Unit
        principal (abst unit T.Int unit `App` int 2)    `shouldNotThrow` Left (BindTypeError $ PatternMismatch unit T.Int)

    context "when given a Case" $ do
      let t = Tuple [int 3, bool False]
          p = tuplePat ["x", "y"]

      it "returns the type which is all the arms identically have" $ do
        principal (Case (bool True) $ return (PVar "x", int 12))      `shouldNotThrowM` T.Int
        principal (Case (bool True) $ return (PVar "x", Var "x" 0 1)) `shouldNotThrowM` T.Bool

        principal (Case t $ return (p, Var "x" 1 2)) `shouldNotThrowM` T.Int
        principal (Case t $ return (p, Var "y" 0 2)) `shouldNotThrowM` T.Bool

        let t = Tagged "a" $ int 88
            p = PVar "x"
            ty = T.variant [("a", T.Int)]
        principal (Case t $ return (p, Var "x" 0 1))          `shouldNotThrow` Left (BareVariant "a" $ int 88)
        principal (Case (Ann t ty) $ return (p, Var "x" 0 1)) `shouldNotThrowM` ty

        let p = PVariant "a" $ PVar "x"
            q = PVariant "nolabel" $ PVar "x"
            s = Var "x" 0 1
            a = return (p, s)
            b = return (q, s)
        principal (Case (Ann t ty) a)        `shouldNotThrowM` T.Int
        principal (Case (Ann t ty) $ a <> a) `shouldNotThrowM` T.Int
        principal (Case (Ann t ty) b)        `shouldNotThrow` Left (BindTypeError $ PatternMismatch q ty)
        principal (Case (Ann t ty) $ a <> b) `shouldNotThrow` Left (BindTypeError $ PatternMismatch q ty)
        principal (Case (Ann t ty) $ b <> a) `shouldNotThrow` Left (BindTypeError $ PatternMismatch q ty)

        let ty = T.variant [("a", T.Int), ("b", T.Unit)]
            q = PVariant "b" $ PVar "x"
            b = return (q, int 10)
        principal (Case (Ann t ty) a)        `shouldNotThrowM` T.Int
        principal (Case (Ann t ty) $ a <> a) `shouldNotThrowM` T.Int
        principal (Case (Ann t ty) b)        `shouldNotThrowM` T.Int
        principal (Case (Ann t ty) $ a <> b) `shouldNotThrowM` T.Int
        principal (Case (Ann t ty) $ b <> a) `shouldNotThrowM` T.Int

      context "when the patterns are inconsistent" $ do
        it "returns an error" $ do
          let p1 = tuplePat ["x", "y", "z"]
          principal (Case t $ return (p, Var "y" 0 2) <> return (p1, Var "z" 0 3)) `shouldNotThrow` Left (BindTypeError $ PatternMismatch p1 $ T.Tuple [T.Int, T.Bool])

          let p = tuplePat ["x"]
          principal (Case (bool True) $ return (p, Var "x" 0 1)) `shouldNotThrow` Left (BindTypeError $ PatternMismatch p T.Bool)

    context "when given a tagged term to which is not annotated its type" $ do
      it "returns an error" $ do
        principal (Tagged "l" $ int 0) `shouldNotThrow` Left (BareVariant "l" $ int 0)

    context "when given a tagged term with an annotation" $ do
      it "returns the annotated type after verifying the term" $ do
        principal (Ann (Tagged "l" $ int 0) $ T.variant []) `shouldNotThrow` Left (VariantError "l" (int 0) Map.empty)

        let ty = T.variant [("l", T.Int)]
        principal (Ann (Tagged "l" $ int 0) ty) `shouldNotThrowM` ty

        let ts = Map.singleton "aaa" T.Int
        principal (Ann (Tagged "l" $ int 0) $ T.Variant ts) `shouldNotThrow` Left (VariantError "l" (int 0) ts)

        let ts = Map.singleton "l" T.Bool
        principal (Ann (Tagged "l" $ int 0) $ T.Variant ts) `shouldNotThrow` Left (TError $ T.Unify T.Int T.Bool)

        let ty = T.variant [("l", T.Int), ("x", T.Unit)]
        principal (Ann (Tagged "l" $ int 0) ty) `shouldNotThrowM` ty

    context "when given annotations" $ do
      it "tests that the type of a term is equal to the annotated type" $ do
        let l = Lit $ Bool False
        principal (Ann l T.Bool) `shouldNotThrowM` T.Bool
        principal (Ann l T.Int)  `shouldNotThrow` Left (TError $ T.Unify T.Bool T.Int)

    context "when given a non-typable term" $ do
      it "returns an error" $ do
        principal (abst (PVar "f") (T.Int T.:->: T.Int) $ abst (PVar "x") T.Bool $ Var "f" 1 2 `App` Var "x" 0 2) `shouldNotThrow` Left (TError $ T.Unify T.Int T.Bool)
        principal (abst (PVar "x") T.Int $ Var "x" 0 1 `App` Var "x" 0 1)                                         `shouldNotThrow` Left (TError $ T.Unify T.Int $ T.Int T.:->: T.fresh 0)

    context "when given an Fold term" $ do
      it "returns the type of the term" $ do
        let ty = T.Rec "X" T.Int
        principal (Fold ty $ int 3) `shouldNotThrowM` ty

        let ty = T.Rec "X" $ T.Var "X" 0 1
        principal (Fold ty $ int 3) `shouldNotThrow` Left (TError $ T.Unify T.Int ty)

    context "when given an Unfold term" $ do
      it "returns the type of the term" $ do
        let ty = T.Rec "X" T.Int
        principal (Unfold ty $ int 3) `shouldNotThrow` Left (TError $ T.Unify T.Int ty)

        let ty = T.Rec "X" $ T.Var "X" 0 1
        principal (Unfold ty $ int 3) `shouldNotThrow` Left (TError $ T.Unify T.Int ty)

        let intlist a = T.variant [("nil", T.Unit), ("cons", T.Tuple [T.Int, a])]
        let ty        = T.Rec "X" $ intlist $ T.Var "X" 0 1
        let ilbody    = intlist ty
        let t         = Ann (Tagged "nil" unit) ilbody
        principal t                       `shouldNotThrowM` ilbody
        principal (Fold ty t)             `shouldNotThrowM` ty
        principal (Unfold ty $ Fold ty t) `shouldNotThrowM` ilbody

  describe "eval" $ do
    context "when given a literal pattern" $ do
      it "evaluates it" $ do
        let p = int 0
        let ty = T.Int
        let t = abst p ty unit
        eval t               `shouldNotThrow` t
        eval (t `App` int 0) `shouldNotThrow` unit

        -- A case where a pattern match fails.
        let t1 = App t $ int 6
        eval t1 `shouldNotThrow` t1

    context "when given a tuple" $ do
      it "evaluates terms in tuples" $ do
        eval (Tuple [Lit $ Int 0])                                              `shouldNotThrow` Tuple [Lit $ Int 0]
        eval (Tuple [abst (PVar "x") T.Int $ Var "x" 0 1])                      `shouldNotThrow` Tuple [abst (PVar "x") T.Int $ Var "x" 0 1]
        eval (Tuple [App (abst (PVar "x") T.Int $ Var "x" 0 1) (Lit $ Int 12)]) `shouldNotThrow` Tuple [Lit $ Int 12]

    context "when given a tuple as the pattern in a lambda abstraction" $ do
      it "evaluates it binding each variable to a item" $ do
        let swap = abst (tuplePat ["a", "b"]) (T.Tuple [T.Int, T.Int]) $ Tuple [Var "b" 0 2, Var "a" 1 2]
        eval (App swap $ Tuple [Lit $ Int 0, Lit $ Int 128]) `shouldNotThrow` Tuple [Lit $ Int 128, Lit $ Int 0]

        let idTuple = abst (PVar "abc") (T.Tuple [T.Int, T.Int]) $ Var "abc" 0 1
        eval (App idTuple $ Tuple [Lit $ Int 0, Lit $ Int 128])            `shouldNotThrow` Tuple [Lit $ Int 0, Lit $ Int 128]
        eval (App swap $ App idTuple $ Tuple [Lit $ Int 0, Lit $ Int 128]) `shouldNotThrow` Tuple [Lit $ Int 128, Lit $ Int 0]

        let fstsnd = abst (PTuple [tuplePat ["a", "b"], tuplePat ["c", "d"]]) (T.Tuple [T.Tuple [T.Int, T.Int], T.Tuple [T.Int, T.Int]]) $ Var "b" 2 4
        eval (App fstsnd $ Tuple [Tuple [Lit $ Int 8, Lit $ Int 16], Tuple [Lit $ Int 32, Lit $ Int 64]]) `shouldNotThrow` Lit (Int 16)

    context "when given annotated values" $ do
      it "removes the annotations" $ do
        eval (Ann (int 12) T.Int) `shouldNotThrow` int 12
        -- Assumes a term well typed. Even if the annotation is wrong, it is ignored.
        eval (Ann (int 12) T.Bool) `shouldNotThrow` int 12

        let t = abst (PVar "x") T.Bool $ int 0
        eval (Ann t $ T.Bool T.:->: T.Int) `shouldNotThrow` t

    context "when given a case-expression" $ do
      it "select one arm whose pattern is appropriate to a term; patterns are tested from up to down" $ do
        let t = int 7
            a = return (PVar "k", Var "k" 0 1)
            b = return (PVar "l", Var "l" 0 1)
        eval (Case t $ a)           `shouldNotThrow` t
        eval (Case t $ a <> a)      `shouldNotThrow` t
        eval (Case t $ b)           `shouldNotThrow` t
        eval (Case t $ b <> b)      `shouldNotThrow` t
        eval (Case t $ a <> b)      `shouldNotThrow` t
        eval (Case t $ b <> a)      `shouldNotThrow` t
        eval (Case t $ a <> a <> a) `shouldNotThrow` t

        let t1 = bool True
        eval (Case t $ return (PVar "k", t1)) `shouldNotThrow` t1

        let tuple = Tuple [t, t1]
            p     = tuplePat ["x", "y"]
            c     = return (p, Var "x" 1 2)
            d     = return (p, Var "y" 0 2)
        eval (Case tuple $ a) `shouldNotThrow` tuple
        eval (Case tuple $ c) `shouldNotThrow` t
        eval (Case tuple $ d) `shouldNotThrow` t1

        eval (App (abst (PVar "x") T.Int $ Var "x" 0 1) (Case tuple $ c)) `shouldNotThrow` t

        let e = return (p, (abst (PVar "z") T.Int $ Var "y" 1 3) `App` Var "x" 1 2)
        eval (App (abst (PVar "x") T.Int $ Var "x" 0 1) (Case tuple $ e)) `shouldNotThrow` t1

    context "when given a tagged term" $ do
      let l = int 5
          t = Tagged "a" l

      context "when no annotation" $ do
        it "does nothing" $ do
          eval t `shouldNotThrow` t

      context "when annotated" $ do
        it "removes the annotation" $ do
          eval (Ann t $ T.variant [("a", T.Int)]) `shouldNotThrow` t

      context "when also given case expression" $ do
        it "performs pattern matching" $ do
          eval (Case t $ return (PVar "x", Var "x" 0 1))                `shouldNotThrow` t
          eval (Case t $ return (PVariant "a" $ PVar "x", Var "x" 0 1)) `shouldNotThrow` l

    context "when given Fold or Unfold" $ do
      it "evaluates it" $ do
        let t0 = int 4
        let ty = T.Rec "X" $ T.Var "X" 0 1
        let t = Fold ty t0
        eval t `shouldNotThrow` t

        let t1 = Fold ty $ abst (PVar "x") T.Int (Var "x" 0 1) `App` t0
        eval t1 `shouldNotThrow` t

        let t1 = Unfold ty $ abst (PVar "x") T.Int (Var "x" 0 1) `App` t0
        let t = Unfold ty t0
        eval t1 `shouldNotThrow` t

        let t = Unfold ty (Fold ty t0)
        eval t `shouldNotThrow` t0

        let t2 = abst (PVar "x") T.Int (Var "x" 0 1) `App` t0
        let t = Unfold ty (Fold ty t2)
        eval t `shouldNotThrow` t0

  describe "reduce" $ do
    it "do beta-reduction" $ do
      let t = Lit $ Bool True
      reduce (PVar "x") (Lit $ Int 8) t `shouldNotThrowM` t

      reduce (PVar "x") (Lit $ Int 12) (Var "y" 1 2) `shouldNotThrowM` Var "y" 0 1
      let t = Var "x" 0 1
      reduce (PVar "x") (Var "y" 1 2) t `shouldNotThrowM` Var "y" 1 2

    context "when given a literal pattern" $ do
      it "tests the value" $ do
        let l1 = int 0
            l2 = int 2
        reduce (int 0) l1 l2 `shouldNotThrowM` l2
        let l1 = int 1
        reduce (int 0) l1 l2 `shouldNotThrow` Left (LiteralMismatch (Int 0) (Int 1))

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
      let idTuple = abst (PVar "t") (T.Tuple [T.Int, T.Int, T.Int]) $ Var "t" 0 1
          int12 = Lit $ Int 12
      evalForPat (tuplePat ["a", "b", "c"]) (idTuple `App` Tuple (replicate 3 int12)) `shouldNotThrow` Tuple (replicate 3 int12)

      let tuple = Tuple $ map Tuple [map int [12, 28], map int [39, 54]]
          p = PTuple [tuplePat ["c", "d"], tuplePat ["e", "f"]]
      evalForPat p tuple                                       `shouldNotThrow` tuple
      evalForPat p (abst (PVar "x") (T.Int) tuple `App` int 2) `shouldNotThrow` tuple

    context "when the type check has not been done" $ do
      it "throws an exception" $ do
        evalForPat (tuplePat ["a", "b", "c"]) (Lit $ Int 1) `shouldThrow` anyException

  describe "shift" $ do
    it "shifts indices in terms" $ do
      let t = int 8
      shift 0 t `shouldBe` t
      shift 1 t `shouldBe` t

      let t = Var "x" 0 1
      shift 0 t `shouldBe` t
      shift 1 t `shouldBe` Var "x" 1 2

      let a = abst (PVar "x") T.Int
      let t = a $ Var "x" 0 1
      shift 0 t `shouldBe` t
      shift 1 t `shouldBe` a (Var "x" 0 2)

      let a = abst (tuplePat ["x", "y"]) T.Int
      let t = a $ Var "y" 0 2
      shift 0 t `shouldBe` t
      shift 1 t `shouldBe` a (Var "y" 0 3)

      let t = a $ Var "x" 1 2
      shift 0 t `shouldBe` t
      shift 1 t `shouldBe` a (Var "x" 1 3)

      let n = int 4
      let t = Var "x" 0 1
      let c t = Case n $ return (PVar "x", t)
      shift 0 (c t) `shouldBe` c t
      shift 1 (c t) `shouldBe` c (Var "x" 0 2)

