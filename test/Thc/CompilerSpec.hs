module Thc.CompilerSpec where

import Test.Hspec

import qualified Data.ByteString.Lazy as B

import qualified Thc.Code as Code
import Thc.Compiler
import Thc.Expr
import qualified Thc.Expr.Indexed as I
import qualified Thc.Type as T

spec :: Spec
spec = do
  describe "compile" $ do
    it "compiles a Term to an executable binary file" $ do
      want <- B.readFile "test/Thc/data/exit178"
      compile (int 178) Code.Darwin Code.Amd64 `shouldBe` Right want

      want <- B.readFile "test/Thc/data/exit81"
      compile (bool True) Code.Darwin Code.Amd64 `shouldBe` Right want

      want <- B.readFile "test/Thc/data/exit81"
      compile (Abs (PVar "x") T.Int (Var "x") `App` int 81) Code.Darwin Code.Amd64 `shouldBe` Right want

      want <- B.readFile "test/Thc/data/exit100"
      compile (Abs (PVar "x") (T.Unit T.:->: T.Unit) (Var "x" `App` unit) `App` Abs (PVar "y") T.Unit unit) Code.Darwin Code.Amd64 `shouldBe` Right want

      want <- B.readFile "test/Thc/data/exit100"
      compile (Abs (PTuple [PVar "x", PVar "y"]) (T.Tuple [T.Int, T.Int]) (Var "y") `App` Tuple [int 200, int 100]) Code.Darwin Code.Amd64 `shouldBe` Right want

      want <- B.readFile "test/Thc/data/exit178"
      compile (Ann (int 178) T.Int) Code.Darwin Code.Amd64 `shouldBe` Right want

      want <- B.readFile "test/Thc/data/exit178"
      compile (Case (Ann (Tagged "a" $ int 178) $ T.variant [("a", T.Int)]) $ return (PVariant "a" $ PVar "x", Var "x")) Code.Darwin Code.Amd64 `shouldBe` Right want

    context "when given an invalid program" $
      it "returns an error" $ do
        compile (Var "x") Code.Darwin Code.Amd64 `shouldBe` Left (Eval $ I.Unbound "x")

        let lit12 = int 12
            p = PTuple []
            ty = T.Int
        compile (Abs p ty lit12) Code.Darwin Code.Amd64 `shouldBe` Left (Type . I.BindTypeError $ I.PatternMismatch p ty)

        let p = tuplePat ["x", "x"]
            ty = T.Tuple [T.Int, T.Int]
        compile (Abs p ty lit12) Code.Darwin Code.Amd64 `shouldBe` Left (Eval . I.BindError $ I.DuplicateVariables p)

        let s = Abs (PVar "x") T.Int $ Var "x" `App` Var "x"
        compile (App s s) Code.Darwin Code.Amd64 `shouldBe` Left (Type $ I.IllTypedApp (I.Var "x" 0 1) T.Int)
