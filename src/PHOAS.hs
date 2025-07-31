{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{-# HLINT ignore "Move brackets to avoid $" #-}
module PHOAS where

import Text.Printf

data Term a
  = Var a
  | Lam (a -> Term a)
  | App (Term a) (Term a)
  | Name String

type Term0 = forall a. Term a

newtype TermBox = TermBox {unTermBox :: Term0}

instance Show TermBox where
  show (TermBox t) = pretty' 0 t

pretty' :: Int -> Term Int -> String
pretty' d (Var n) = printf "x%d" n
pretty' d (Lam f) = printf "λx%d.%s" d (pretty' (d + 1) (f d))
pretty' d (App t1 t2) = "(" ++ pretty' d t1 ++ " " ++ pretty' d t2 ++ ")"
pretty' d (Name s) = s

cntBind' :: Term () -> Int
cntBind' (Var _) = 0
cntBind' (Lam f) = 1 + cntBind' (f ())
cntBind' (App t1 t2) = cntBind' t1 + cntBind' t2
cntBind' (Name _) = 0

cntBind :: Term0 -> Int
cntBind t = cntBind' (t @())

-- substitution

type Term1 = forall a. a -> Term a

subst' :: forall a. Term (Term a) -> Term a
subst' (Var e) = e
subst' (Lam f) = Lam $ \x -> subst' (f (Var x))
subst' (App t1 t2) = App (subst' t1) (subst' t2)
subst' (Name s) = Name s

subst :: Term1 -> Term0 -> Term0
subst f t = subst' (f t)

-- reduce strategy

data ReduceResult t = Step t | Stop t

-- reduceCBN :: Term0 -> ReduceResult Term0
-- reduceCBN (Var x) = Stop (Var x)
-- reduceCBN (Lam f) = Stop (Lam f)
-- reduceCBN (App (Lam f) t2) = Step (subst f t2)
-- reduceCBN (App (Name n) t2) =
--   case reduceCBN t2 of
--     Step t2' -> Step (App (Name n) t2')
--     Stop t2' -> Stop (App (Name n) t2')
-- reduceCBN (App t1 t2) =
--   case reduceCBN t1 of
--     Step t1' -> Step (App t1' t2)
--     Stop t1' -> Step (App t1' t2)
-- reduceCBN (Name n) = Stop (Name n)

-- useful functions for Term0

(@) :: Term a -> Term a -> Term a
(@) = App

-- example terms

zro :: forall a. Term a
zro = Lam $ \f -> Lam $ \z -> Var z

suc :: forall a. Term a
suc = Lam $ \n -> Lam $ \f -> Lam $ \z -> Var f @ (Var n @ Var f @ Var z)

nat :: Int -> forall a. Term a
nat 0 = zro
nat n | n > 0 = Lam $ \f -> Lam $ \z -> Var f @ (nat (n - 1) @ Var f @ Var z)
nat _ = error "nat: negative number"

tru :: forall a. Term a
tru = Lam $ \x -> Lam $ \y -> Var x

fls :: forall a. Term a
fls = Lam $ \x -> Lam $ \y -> Var y

ite :: forall a. Term a
ite = Lam $ \p -> Lam $ \t -> Lam $ \f -> Var p @ Var t @ Var f

cst :: forall a. Term a
cst = Lam $ \x -> Lam $ \y -> Var x

isz :: forall a. Term a
isz = Lam $ \n -> Var n @ (cst @ fls) @ tru

ifz :: forall a. Term a
ifz = Lam $ \n -> Lam $ \z -> Lam $ \nz -> Var n @ (cst @ Var nz) @ Var z

pair :: forall a. Term a
pair = Lam $ \x -> Lam $ \y -> Lam $ \f -> Var f @ Var x @ Var y

fst_ :: forall a. Term a
fst_ = Lam $ \p -> Var p @ tru

snd_ :: forall a. Term a
snd_ = Lam $ \p -> Var p @ fls

prd :: forall a. Term a
prd = Lam $ \n -> snd_ @ (Var n @ (Lam $ \p -> pair @ (suc @ (fst_ @ Var p)) @ (fst_ @ Var p)) @ (pair @ zro @ zro))

add :: forall a. Term a
add = Lam $ \m -> Lam $ \n -> Lam $ \f -> Lam $ \z -> Var m @ Var f @ (Var n @ Var f @ Var z)

mul :: forall a. Term a
mul = Lam $ \m -> Lam $ \n -> Lam $ \f -> Lam $ \z -> Var m @ (Var n @ Var f @ Var z)

fct :: forall a. Term a
fct = y @ (Lam $ \f -> Lam $ \n -> ifz @ Var n @ nat 1 @ (mul @ Var n @ (Var f @ (prd @ Var n))))

self :: forall a. Term a
self = Lam $ \x -> Var x @ Var x

omega :: forall a. Term a
omega = self @ self

y :: forall a. Term a
y = Lam $ \f -> (Lam $ \x -> Var f @ (Var x @ Var x)) @ (Lam $ \x -> Var f @ (Var x @ Var x))

mu :: forall a. Term a
mu = Lam $ \t -> (y @ (Lam $ \f -> Lam $ \n -> ifz @ (Var t @ Var n) @ Var n @ (Var f @ (suc @ Var n)))) @ zro