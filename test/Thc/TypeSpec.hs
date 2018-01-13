module Thc.TypeSpec where

import Test.Hspec
import Test.QuickCheck hiding (variant)

import Control.Applicative

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
