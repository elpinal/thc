module Thc.Expr.Indexed
  ( Term(..)
  , eval
  , E.Literal(..)
  , fromLiteral
  , fromNamed
  , typeOf
  ) where

import Control.Arrow

import qualified Thc.Expr as E
import qualified Thc.Type as T

data Term =
    -- |
    -- Name hint, de Bruijn index, and the surrounding context's length for debug
    Var String Int Int
  | Abs E.Pattern T.Type Term
  | App Term Term
  | Lit E.Literal
  | Tuple [Term]
  deriving (Eq, Show)

fromNamed :: E.Term -> Maybe Term
fromNamed = fromNamed' emptyContext

fromNamed' :: Context -> E.Term -> Maybe Term

fromNamed' ctx (E.Var i) = do
  x <- name2index i ctx
  return $ Var i x $ length ctx

fromNamed' ctx (E.Abs p @ (E.PVar i) ty t) = Abs p ty <$> fromNamed' (addName i ty ctx) t
fromNamed' ctx (E.Abs p @ (E.PTuple is) ty @ (T.Tuple tys) t)
  | length is == length tys = do
  let xs = zip is tys
  let ctx' = addNames xs ctx
  Abs p ty <$> fromNamed' ctx' t

fromNamed' ctx (E.App t1 t2) = do
  u1 <- fromNamed' ctx t1
  u2 <- fromNamed' ctx t2
  return $ App u1 u2

fromNamed' ctx (E.Lit l) = return $ Lit l
fromNamed' ctx (E.Tuple ts) = Tuple <$> mapM (fromNamed' ctx) ts

bindPattern :: E.Pattern -> T.Type -> Context -> Maybe Context
bindPattern (E.PVar i) ty ctx = return $ addName i ty ctx
bindPattern (E.PTuple is) (T.Tuple ts) ctx
  | length is == length ts = return $ addNames (zip is ts) ctx
  | otherwise              = Nothing
bindPattern (E.PTuple is) ty _ = Nothing -- Note that type variables are currently not supported.

type Context = [(String, T.Type)]

emptyContext :: Context
emptyContext = []

addNames :: [(String, T.Type)] -> Context -> Context
addNames xs ctx = xs ++ ctx

addName :: String -> T.Type -> Context -> Context
addName i ty ctx = (i, ty) : ctx

name2index :: String -> Context -> Maybe Int
name2index i [] = Nothing
name2index i (x : xs)
  | i == fst x = return 0
  | otherwise  = (1 +) <$> name2index i xs

shift :: Int -> Term -> Term
shift d = walk 0
  where
    walk :: Int -> Term -> Term
    walk c (Var i x n)
      | x >= c    = Var i (x + d) (n + d) -- free
      | otherwise = Var i x $ n + d       -- bound
    walk c (Abs i ty t') = Abs i ty $ walk (c + 1) t'
    walk c (App t1 t2) = App (walk c t1) (walk c t2)
    walk c (Tuple ts) = Tuple $ walk c `map` ts
    walk c l @ (Lit _) = l

subst :: Int -> Term -> Term -> Term
subst j s = walk 0
  where
    walk :: Int -> Term -> Term
    walk c t' @ (Var i x n)
      | x == j + c = shift c s
      | otherwise  = t'
    walk c (Abs i ty t') = Abs i ty $ walk (c + 1) t'
    walk c (App t1 t2) = App (walk c t1) (walk c t2)
    walk c (Tuple ts) = Tuple $ walk c `map` ts
    walk c l @ (Lit _) = l

-- subst
substTop :: (Term, Term) -> Term
substTop = subst 0 . shift 1 *** id >>> app >>> shift (-1)

-- | Evaluates a 'Term' to its normal form.
eval :: Term -> Term
eval t = maybe t eval $ eval1 t

eval1 :: Term -> Maybe Term
eval1 (App (Abs _ _ t1) t2) = return $ substTop (t2, t1)
eval1 (App t1 t2) = flip App t2 <$> eval1 t1
eval1 (Tuple ts) = Tuple <$> f ts
  where
    f :: [Term] -> Maybe [Term]
    f [] = Nothing
    f (t : ts) = case eval1 t of
      Just t' -> return $ t' : ts
      Nothing -> (t :) <$> f ts
eval1 _ = Nothing

fromLiteral :: Term -> Maybe E.Literal
fromLiteral (Lit l) = return l
fromLiteral _ = Nothing

typeOf :: Term -> Maybe T.Type
typeOf = typeOf' emptyContext

typeOf' :: Context -> Term -> Maybe T.Type
typeOf' ctx (Var _ x _) = return $ getTypeFromContext ctx x
typeOf' ctx (Abs p ty1 t) = do
  ctx' <- bindPattern p ty1 ctx
  ty2 <- typeOf' ctx' t
  return $ ty1 T.:->: ty2
typeOf' ctx (App t1 t2) = do
  ty1 <- typeOf' ctx t1
  ty2 <- typeOf' ctx t2
  case ty1 of
    u1 T.:->: u2 | u1 == ty2 -> return u2
    _                        -> Nothing
typeOf' ctx (Lit l) = return $ E.typeOfLiteral l
typeOf' ctx (Tuple ts) = T.Tuple <$> mapM (typeOf' ctx) ts

getTypeFromContext :: Context -> Int -> T.Type
getTypeFromContext ctx n = snd $ ctx !! n
