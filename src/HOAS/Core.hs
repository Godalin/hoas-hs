{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators  #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module HOAS.Core where

import           Text.Printf

data Binder a b = Bind Int ([Var a] -> b)

data Var a = Var a | Idx Int

data Bind t = A t | B (Var t)

type (|-) = Binder
infixr |-

subst :: (a |- b) -> [a] -> Maybe b
subst (Bind nb f) xs = if nb == length xs then return (f $ map Var xs) else Nothing



-- |
-- Example

data Term = Abs (Term |- Term) | App Term Term

pretty :: Int -> Term -> String
pretty d (Abs (Bind _ f)) = printf "(%s)"
pretty d (App t1 t2)      = printf "(%s ∙ %s)" (pretty d t1) (pretty d t2)


eg1 :: Int |- Int |- Int
eg1 = Bind 2 \[Var x, Var y] -> Bind 1 \[Var z] -> z
