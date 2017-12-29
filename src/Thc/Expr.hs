module Thc.Expr
  ( Term(..)
  , Literal(..)
  , typeOfLiteral
  ) where

import qualified Thc.Type as T

data Term =
    Var String
  | Abs String T.Type Term
  | App Term Term
  | Lit Literal
  | Tuple [Term]

data Literal =
    Bool Bool
  | Int Int
  | Unit
  deriving (Eq, Show)

typeOfLiteral :: Literal -> T.Type
typeOfLiteral (Bool _) = T.Bool
typeOfLiteral (Int _) = T.Int
typeOfLiteral Unit = T.Unit
