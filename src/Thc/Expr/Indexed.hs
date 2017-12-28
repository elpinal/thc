module Thc.Expr.Indexed
  ( Term(..)
  , eval
  , E.Literal(..)
  , fromLiteral
  , fromNamed
  ) where

import Control.Arrow

import qualified Thc.Expr as E

data Term =
    Var String Int Int
  | Abs String Term
  | App Term Term
  | Lit E.Literal
  deriving (Eq, Show)

fromNamed :: E.Term -> Maybe Term
fromNamed = fromNamed' emptyContext

fromNamed' :: Context -> E.Term -> Maybe Term

fromNamed' ctx (E.Var i) = do
  x <- (name2index i ctx)
  return $ Var i x $ length ctx

fromNamed' ctx (E.Abs i t) = Abs i <$> fromNamed' (addName i ctx) t

fromNamed' ctx (E.App t1 t2) = do
  u1 <- fromNamed' ctx t1
  u2 <- fromNamed' ctx t2
  return $ App u1 u2

fromNamed' ctx (E.Lit l) = return $ Lit l

type Context = [String]

emptyContext :: Context
emptyContext = []

addName :: String -> Context -> Context
addName i ctx = i : ctx

name2index :: String -> Context -> Maybe Int
name2index i [] = Nothing
name2index i (x : xs)
  | i == x    = return 0
  | otherwise = (1 +) <$> name2index i xs

shift :: Int -> Term -> Term
shift d t = walk 0 t
  where
    walk :: Int -> Term -> Term
    walk c (Var i x n)
      | x >= c    = Var i (x + d) (n + d) -- free
      | otherwise = Var i x $ n + d       -- bound
    walk c (Abs i t') = Abs i $ walk (c + 1) t'
    walk c (App t1 t2) = App (walk c t1) (walk c t2)
    walk c l @ (Lit _) = l

subst :: Int -> Term -> Term -> Term
subst j s t = walk 0 t
  where
    walk :: Int -> Term -> Term
    walk c t' @ (Var i x n)
      | x == j + c = shift c s
      | otherwise  = t'
    walk c (Abs i t') = Abs i $ walk (c + 1) t'
    walk c (App t1 t2) = App (walk c t1) (walk c t2)
    walk c l @ (Lit _) = l

-- subst
substTop :: (Term, Term) -> Term
substTop = subst 0 . shift 1 *** id >>> app >>> shift (-1)

-- | Evaluates a 'Term' to its normal form.
eval :: Term -> Term
eval t = maybe t eval $ eval1 t

eval1 :: Term -> Maybe Term
eval1 (App (Abs _ t1) t2) = return $ substTop (t2, t1)
eval1 (App t1 t2) = flip App t2 <$> eval1 t1
eval1 _ = Nothing

fromLiteral :: Term -> Maybe E.Literal
fromLiteral (Lit l) = return l
fromLiteral _ = Nothing
