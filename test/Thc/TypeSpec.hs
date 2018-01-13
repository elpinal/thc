module Thc.TypeSpec where

import Test.Hspec
import Test.QuickCheck hiding (variant)

import Control.Applicative
import Control.Monad

import Thc.Type

instance Arbitrary TypeId where
  arbitrary = IdString <$> arbitrary

instance Arbitrary Type where
  arbitrary = oneof $
    map return [Int, Bool, Unit] ++
    [ Id <$> arbitrary
    , liftA2 (:->:) arbitrary arbitrary
    , liftA3 Var arbitrary arbitrary arbitrary
    , liftA2 Rec arbitrary arbitrary
    -- TODO: diverge; need investigation
    -- , Tuple <$> arbitrary
    -- , Record <$> arbitrary
    -- , variant <$> arbitrary
    ]

shouldCommute :: (HasCallStack, Eq b, Show b) => (a -> a -> b) -> a -> a -> b -> Expectation
shouldCommute f x y z = do
  f x y `shouldBe` z
  f y x `shouldBe` z

spec :: Spec
spec = do
  describe "varBind" $ do
    context "when both types are the same" $ do
      it "returns the empty substitution" $ property $ \i ->
        varBind i (Id i) === return emptySubst

    context "when both types are not the same" $ do
      it "returns a substitution from the type variable to the type" $ property $
        \i -> forAll (suchThat arbitrary (/= Id i)) $ \t ->
          varBind i t === return (i |-> t)

  describe "mgu" $ do
    context "when given two arrow type" $ do
      it "returns the most general unifier" $ do
        shouldCommute mgu (Int :->: idString "X") (Int :->: idString "X")          $ return emptySubst
        shouldCommute mgu (Int :->: idString "X") (idString "X" :->: Int)          $ return $ IdString "X" |-> Int
        shouldCommute mgu (idString "Y" :->: idString "X") (idString "X" :->: Int) $ IdString "Y" |-> Int `merge` IdString "X" |-> Int
        shouldCommute mgu (idString "X" :->: idString "Y") (Int :->: idString "X") $ IdString "Y" |-> Int `merge` IdString "X" |-> Int
        shouldCommute mgu (idString "X" :->: idString "Y") (idString "X" :->: Int) $ return $ IdString "Y" |-> Int

    context "when either or both of the two types are type variable" $ do
      it "returns the result of varBind" $ do
        shouldCommute mgu (idString "X") (idString "X") $ return emptySubst

        mgu (idString "X") (idString "Y") `shouldBe` return (IdString "X" |-> idString "Y")
        mgu (idString "Y") (idString "X") `shouldBe` return (IdString "Y" |-> idString "X")

        shouldCommute mgu (idString "X" :->: idString "Y") (idString "X") $ return $ IdString "X" |-> (idString "X" :->: idString "Y")

    context "when two types cannot be unified" $ do
      it "returns an error" $ do
        mgu (idString "X" :->: idString "Y") Int `shouldBe` Left (Unify (idString "X" :->: idString "Y") Int)

  describe "unify" $ do
    it "returns the most general unifier that is the principal solution of a set of constraints" $ do
      (unify . fromList) []                    `shouldBe` return emptySubst
      (unify . fromList) [(Int, Int)]          `shouldBe` return emptySubst
      (unify . fromList) [(idString "X", Int)] `shouldBe` return (IdString "X" |-> Int)

      let l = [(idString "X", Int), (idString "Y", idString "X" :->: idString "X")]
      (unify . fromList) l `shouldBe` (IdString "X" |-> Int `merge` IdString "Y" |-> (Int :->: Int))

      let l = [(Int :->: Int, idString "X" :->: idString "Y")]
      (unify . fromList) l `shouldBe` (IdString "X" |-> Int `merge` IdString "Y" |-> Int)

      let l = [ (idString "X" :->: idString "Y", idString "Y" :->: idString "Z")
              , (idString "Z"                  , idString "U" :->: idString "W")
              ]
      let s = let ty = idString "U" :->: idString "W" in
              foldM merge emptySubst
                [ IdString "X" |-> ty
                , IdString "Y" |-> ty
                , IdString "Z" |-> ty
                ]
      (unify . fromList) l `shouldBe` s

      let ty = Int :->: idString "Y"
      (unify . fromList) [(Int, ty)] `shouldBe` Left (Unify Int ty)

    context "when given recursive constraints" $ do
      it "treats them as well as ones that has no recursive constraints" $ do
        let ty = Int :->: idString "Y"
        (unify . fromList) [(idString "Y", ty)] `shouldBe` return (IdString "Y" |-> ty)
