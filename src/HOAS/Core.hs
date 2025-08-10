{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HOAS.Core where

import           Control.Monad
import           Control.Monad.Except
import           Data.List
import           Text.Printf

data Binder a b = Bind Int Int ([Var a] -> b)

data Var a = Var a | Idx Int

instance Show a => Show (Var a) where
  show (Var a) = show a
  show (Idx n) = printf "x%d" n

type (|-) = Binder
infixr |-

instance Show b => Show (Binder a b) where
  show (Bind start nb f) = printf "%s |- %s" vars (show $ f (map Idx ids)) where
    ids = [start .. start + nb - 1]
    vars = intercalate "," $ map (("x" ++) . show) ids


-- | substitution of bound variables
subst :: (a |- b) -> [a] -> Except String b
subst (Bind _ nb f) xs = do
  guard (nb == length xs)
  return (f $ map Var xs)

-- | a language of type b should embed a variable in to it
class Variable a b where
  var :: Var a -> b



-- |
-- examples

data Term
  = VarT (Var Term)
  | AbsT (Term |- Term)
  | AppT Term Term
  deriving (Show)

instance Variable Term Term where
  var (Var t) = t
  var (Idx i) = VarT (Idx i)

eg1 :: Term
eg1 = AbsT (Bind 0 2 \[x, y] -> var x)

eg2 :: Term
eg2 = AbsT (Bind 0 2 \[x, y] -> var x) @ AbsT (Bind 0 1 \[x] -> var x)

lam f = AbsT (Bind 0 1 \[x] -> f x)

(@) = AppT

eg3 :: Term
eg3 = lam \x -> lam \y -> var x @ var y
