module Thc.Expr
  ( Term(..)
  , Literal(..)
  , Lit(..)
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
  | Tagged String Term

data Literal =
    Bool Bool
  | Int Int
  | Unit
  deriving (Eq, Show)

class Lit t where
  bool :: Bool -> t
  int :: Int -> t
  unit :: t

instance Lit Term where
  bool = Lit . Bool
  int = Lit . Int
  unit = Lit Unit

typeOfLiteral :: Literal -> T.Type
typeOfLiteral (Bool _) = T.Bool
typeOfLiteral (Int _) = T.Int
typeOfLiteral Unit = T.Unit

data Pattern =
  -- | A pattern which matches everything. @PVar i@ binds stuff to @i@
    PVar String
  -- | A pattern which matches a tuple. @PTuple ps@ binds each item of the tuple to each pattern of @ps@
  | PTuple [Pattern]
  -- | A pattern which matches a variant. @PVariant i p@ binds a term tagged with @i@ to @p@
  | PVariant String Pattern
  deriving (Eq, Show)

bounds :: Pattern -> [String]
bounds (PVar i) = [i]
bounds (PTuple ps) = concatMap bounds ps
bounds (PVariant i p) = bounds p

tuplePat :: [String] -> Pattern
tuplePat = PTuple . map PVar
