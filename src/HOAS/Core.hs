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

data Binder a b = Bind Int ([Var a] -> b)

data Var a = Var a | Idx Int
  deriving Show

data Bind t = A t | B (Var t)

type (|-) = Binder
infixr |-

instance Show b => Show (Binder a b) where
  show (Bind nb f) = printf "%s |- %s" vars (show $ f (map Idx ids)) where
    ids = [0 .. nb-1]
    vars = intercalate "," $ map (("x" ++) . show) ids


-- | substitution of bound variables
subst :: (a |- b) -> [a] -> Except String b
subst (Bind nb f) xs = do
  guard (nb == length xs)
  return (f $ map Var xs)

-- | a language of type b should embed a variable in to it
class Variable a b where
  unvar :: Var a -> b



-- |
-- examples

data Term
  = VarT (Var Term)
  | AbsT (Term |- Term)
  | AppT Term Term
  deriving (Show)

instance Variable Term Term where
  unvar (Var t) = t
  unvar (Idx i) = VarT (Idx i)

eg1 :: Term
eg1 = AbsT (Bind 2 \[x, y] -> unvar x)

eg2 :: Term
eg2 = AbsT (Bind 2 \[x, y] -> unvar x) @ AbsT (Bind 1 \[x] -> unvar x)

lam f = AbsT (Bind 1 \[x] -> f x)

(@) = AppT

eg3 :: Term
eg3 = lam \x -> lam \y -> unvar x @ unvar y
