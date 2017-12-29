module Thc.Expr
  ( Term(..)
  , Literal(..)
  ) where

import qualified Thc.Type as T

data Term =
    Var String
  | Abs String T.Type Term
  | App Term Term
  | Lit Literal

data Literal =
    Bool Bool
  | Int Int
  deriving (Eq, Show)
