-- Everything you want about untyped lambda calculus
-- is in this file
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module UTLC where

import Text.Printf
import Prelude hiding ((*))

reduceList :: Term -> IO ()
reduceList t = mapM_ print (reduceIter reduceAll 200 t)

data Term
  = App Term Term
  | Lam (Term -> Term)
  | Var Int -- for printing
  | Name String -- for named terms, not used in reduction

(@) :: Term -> Term -> Term
(@) = App



instance Show Term where
  show = prettyTerm 0



-- Simple Pretty Printer

prettyTerm :: Int -> Term -> String
prettyTerm d (App t1 t2) = printf "(%s %s)" (prettyTerm d t1) (prettyTerm d t2)
prettyTerm d (Lam f) = printf "λx%d.%s" d (prettyTerm (d + 1) (f (Var d)))
prettyTerm _ (Var n) = printf "x%d" n
prettyTerm _ (Name name) = name



-- Reduction strategies

data ReduceResult
  = Stop Term
  | Step Term
  | Error String
  deriving (Show)

-- Small Step Reduction
reduceAll :: Term -> ReduceResult
reduceAll (App (Lam f) arg) =
  case reduceAll arg of
    Stop arg' -> Step (f arg')
    Step arg' -> Step (App (Lam f) arg')
    Error err -> Error err
reduceAll (App (Name f) arg) =
  case reduceAll arg of
    Stop arg' -> Stop (App (Name f) arg')
    Step arg' -> Step (App (Name f) arg')
    Error err -> Error err
reduceAll (App t1 t2) =
  case reduceAll t1 of
    Stop t1' -> case reduceAll t2 of
      Stop t2' -> Step (App t1' t2')
      Step t2' -> Step (App t1' t2')
      Error err -> Error err
    Step t1' -> Step (App t1' t2)
    Error err -> Error err
reduceAll (Lam f) = Stop (Lam f)
reduceAll (Name name) = Stop (Name name)
reduceAll _ = Error "invalid term"



reduceCBV :: Term -> ReduceResult
reduceCBV (App (Lam f) arg) =
  Stop (f arg)
reduceCBV (App t1 t2) =
  case (reduceCBV t1, reduceCBV t2) of
    (Stop t1', Stop t2') -> Stop (App t1' t2')
    (Step t1', _) -> Step (App t1' t2)
    (_, Step t2') -> Step (App t1 t2')
    (Error err, _) -> Error err
    (_, Error err) -> Error err
reduceCBV (Lam f) = Stop (Lam f)
reduceCBV (Name name) = Stop (Name name)
reduceCBV _ = Error "invalid term"



-- reduce iterator
reduceIter :: (Term -> ReduceResult) -> Int -> Term -> [Term]
reduceIter reducef d t = t : go d t where
  go 0 _ = []
  go d t = case reducef t of
    Step t' -> t' : go (d - 1) t'
    _ -> []



-- example terms

zro :: Term
zro = Lam $ \f -> Lam $ \z -> z

suc :: Term
suc = Lam $ \n -> Lam $ \f -> Lam $ \z -> f @ (n @ f @ z)

nat :: Int -> Term
nat 0 = zro
nat n | n > 0 = Lam $ \f -> Lam $ \z -> f @ (nat (n - 1) @ f @ z)
nat _ = error "nat: negative number"

repnat :: Int -> Term
repnat n = (nat n) @ (Name "s") @ (Name "z")

tru :: Term
tru = Lam $ \x -> Lam $ \y -> x

fls :: Term
fls = Lam $ \x -> Lam $ \y -> y

ite :: Term
ite = Lam $ \p -> Lam $ \t -> Lam $ \f -> p @ t @ f

cst :: Term
cst = Lam $ \x -> Lam $ \y -> x

ifz :: Term
ifz = Lam $ \n -> Lam $ \z -> Lam $ \nz -> n @ (cst @ nz) @ z

pair :: Term
pair = Lam $ \x -> Lam $ \y -> Lam $ \f -> f @ x @ y

fst_ :: Term
fst_ = Lam $ \p -> p @ (Lam $ \x -> Lam $ \y -> x)

snd_ :: Term
snd_ = Lam $ \p -> p @ (Lam $ \x -> Lam $ \y -> y)

prd :: Term
prd = (Lam $ \n -> snd_ @ (n @ (Lam $ \p -> pair @ (suc @ (fst_ @ p)) @ (fst_ @ p) ) @ (pair @ zro @ zro)))

add :: Term
add = Lam $ \m -> Lam $ \n -> Lam $ \f -> Lam $ \z -> m @ f @ (n @ f @ z)

mul :: Term
mul = Lam $ \m -> Lam $ \n -> Lam $ \f -> Lam $ \z -> m @ (n @ f) @ z

fct :: Term
fct = y @ (Lam $ \f -> (Lam $ \n -> ifz @ n @ (nat 1) @ (mul @ n @ (f @ (prd @ n)))))

self :: Term
self = Lam (\x -> x @ x)

omega :: Term
omega = self @ self

y :: Term
y = Lam (\f -> (Lam (\x -> f @ (x @ x))) @ (Lam (\x -> f @ (x @ x))))

fix :: Term
fix = y