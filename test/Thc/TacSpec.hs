module Thc.TacSpec where

import Test.Hspec

import qualified Thc.Expr as Expr
import Thc.Tac

spec :: Spec
spec = do
  describe "fromExpr" $
    it "translates Expr to Tac" $ do
      fromExpr (Expr.Var "x") `shouldBe` Right (Ret $ Var "x")
      fromExpr (Expr.Var "7") `shouldBe` Right (Ret $ Var "7")
