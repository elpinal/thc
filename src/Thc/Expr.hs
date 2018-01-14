module Thc.Expr
  ( Term(..)
  , abst
  , Literal(..)
  , Lit(..)
  , typeOfLiteral
  , Pattern(..)
  , tuplePat
  , bounds
  , nbounds
  ) where

import qualified Data.List.NonEmpty as NonEmpty

import qualified Thc.Type as T

data Term =
    Var String
  | Abs Pattern (Maybe T.Type) Term
  | App Term Term
  | Lit Literal
  | Tuple [Term]
  | Record [(String, Term)]
  | Ann Term T.Type
  | Tagged String Term
  | Case Term (NonEmpty.NonEmpty (Pattern, Term))

abst :: Pattern -> T.Type -> Term -> Term
abst p ty t = Abs p (return ty) t

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

instance Lit Pattern where
  bool = PLiteral . Bool
  int = PLiteral . Int
  unit = PLiteral Unit

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
  -- | A pattern for a literal.
  | PLiteral Literal
  deriving (Eq, Show)

-- |
-- @bounds p@ returns the list of the variables bound by @p@.
--
-- >>> bounds (PVar "x")
-- ["x"]
-- >>> bounds (tuplePat ["a", "b"])
-- ["a","b"]
-- >>> bounds (PLiteral $ Int 2)
-- []
--
-- The returned list may have duplicates.
--
-- >>> bounds (PVariant "a" $ tuplePat ["x", "y", "x"])
-- ["x","y","x"]
bounds :: Pattern -> [String]
bounds (PVar i) = [i]
bounds (PTuple ps) = concatMap bounds ps
bounds (PVariant i p) = bounds p
bounds (PLiteral l) = []

nbounds :: Pattern -> Int
nbounds = length . bounds

tuplePat :: [String] -> Pattern
tuplePat = PTuple . map PVar
