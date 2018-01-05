module Thc.Expr
  ( Term(..)
  , Literal(..)
  , bool
  , int
  , unit
  , typeOfLiteral
  , Pattern(..)
  , tuplePat
  , bounds
  ) where

import qualified Thc.Type as T

data Term =
    Var String
  | Abs Pattern T.Type Term
  | App Term Term
  | Lit Literal
  | Tuple [Term]
  | Record [(String, Term)]
  | Ann Term T.Type

data Literal =
    Bool Bool
  | Int Int
  | Unit
  deriving (Eq, Show)

bool :: Bool -> Term
bool = Lit . Bool

int :: Int -> Term
int = Lit . Int

unit :: Term
unit = Lit Unit

typeOfLiteral :: Literal -> T.Type
typeOfLiteral (Bool _) = T.Bool
typeOfLiteral (Int _) = T.Int
typeOfLiteral Unit = T.Unit

data Pattern =
    PVar String
  | PTuple [Pattern]
  deriving (Eq, Show)

bounds :: Pattern -> [String]
bounds (PVar i) = [i]
bounds (PTuple ps) = concatMap bounds ps

tuplePat :: [String] -> Pattern
tuplePat = PTuple . map PVar
