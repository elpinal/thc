module Thc.CompilerSpec where

import Test.Hspec

import qualified Data.ByteString.Lazy as B

import qualified Thc.Code as Code
import Thc.Compiler
import Thc.Expr
import qualified Thc.Expr.Indexed as I
import qualified Thc.Type as T

shouldNotThrow :: (HasCallStack, Eq a, Show a) => IO a -> a -> Expectation
shouldNotThrow x y = do
  x' <- x
  x' `shouldBe` y

spec :: Spec
spec = do
  describe "compile" $ do
    it "compiles a Term to an executable binary file" $ do
      want <- B.readFile "test/Thc/data/exit178"
      compile (int 178) Code.Darwin Code.Amd64 `shouldNotThrow` Right want

      want <- B.readFile "test/Thc/data/exit81"
      compile (bool True) Code.Darwin Code.Amd64 `shouldNotThrow` Right want

      want <- B.readFile "test/Thc/data/exit81"
      compile (abst (PVar "x") T.Int (Var "x") `App` int 81) Code.Darwin Code.Amd64 `shouldNotThrow` Right want

      want <- B.readFile "test/Thc/data/exit100"
      compile (abst (PVar "x") (T.Unit T.:->: T.Unit) (Var "x" `App` unit) `App` abst (PVar "y") T.Unit unit) Code.Darwin Code.Amd64 `shouldNotThrow` Right want

      want <- B.readFile "test/Thc/data/exit100"
      compile (abst (PTuple [PVar "x", PVar "y"]) (T.Tuple [T.Int, T.Int]) (Var "y") `App` Tuple [int 200, int 100]) Code.Darwin Code.Amd64 `shouldNotThrow` Right want

      want <- B.readFile "test/Thc/data/exit178"
      compile (Ann (int 178) T.Int) Code.Darwin Code.Amd64 `shouldNotThrow` Right want

      want <- B.readFile "test/Thc/data/exit178"
      compile (Case (Ann (Tagged "a" $ int 178) $ T.variant [("a", T.Int)]) $ return (PVariant "a" $ PVar "x", Var "x")) Code.Darwin Code.Amd64 `shouldNotThrow` Right want

    context "when given an invalid program" $
      it "returns an error" $ do
        compile (Var "x") Code.Darwin Code.Amd64 `shouldNotThrow` Left (Eval $ I.Unbound "x")

        let lit12 = int 12
            p = PTuple []
            ty = T.Int
        compile (abst p ty lit12) Code.Darwin Code.Amd64 `shouldNotThrow` Left (Type . I.BindTypeError $ I.PatternMismatch p $ T.toScheme ty)

        let p = tuplePat ["x", "x"]
            ty = T.Tuple [T.Int, T.Int]
        compile (abst p ty lit12) Code.Darwin Code.Amd64 `shouldNotThrow` Left (Eval . I.BindError $ I.DuplicateVariables p)

        let s = abst (PVar "x") T.Int $ Var "x" `App` Var "x"
        compile (App s s) Code.Darwin Code.Amd64 `shouldNotThrow` Left (Type . I.TError . T.Unify T.Int $ T.Int T.:->: T.fresh 1)
