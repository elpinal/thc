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
    , fmap Tuple $ replicate <$> arbitrary <*> arbitrary
    , fmap Record $ replicate <$> arbitrary <*> arbitrary
    , fmap variant $ replicate <$> arbitrary <*> arbitrary
    ]

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
