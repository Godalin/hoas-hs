-- Everything you want about untyped lambda calculus
-- is in this file

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE OverloadedStrings #-}

module UTLC where

import Data.String
import Text.Printf
import Control.Monad 

reduceList :: Term -> IO ()
reduceList t = mapM_ print (reduceIter reduceCBV 200 t)

reduceListStepper :: Term -> IO ()
reduceListStepper t = mapM (print >=> const getLine >=> const (return 1))
  (reduceIterInfi reduceCBN t) >>= (print . sum)

reduceListInfiAuto :: Term -> IO ()
reduceListInfiAuto t = mapM (print >=> const (putStrLn "") >=> const (return 1))
  (reduceIterInfi reduceCBN t) >>= (print . sum)

data Term
  = App !Term !Term
  | Lam (Term -> Term)
  | Var Int -- for printing
  | Name String -- for named terms, not used in reduction

(@) :: Term -> Term -> Term
(@) = App



instance Show Term where
  show = show . toFO
  -- show = prettyTerm 0



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

-- small step reductions

-- call by value
reduceCBV :: Term -> ReduceResult
reduceCBV (Lam f) = Stop (Lam f)
reduceCBV (Name name) = Stop (Name name)
reduceCBV (App (Lam f) arg) =
  case reduceCBV arg of
    Stop arg' -> Step (f arg')
    Step arg' -> Step (App (Lam f) arg')
    Error err -> Error err
reduceCBV (App (Name f) arg) =
  case reduceCBV arg of
    Stop arg' -> Stop (App (Name f) arg')
    Step arg' -> Step (App (Name f) arg')
    Error err -> Error err
reduceCBV (App t1 t2) =
  case reduceCBV t1 of
    Stop t1' -> Step (App t1' t2)
    Step t1' -> Step (App t1' t2)
    Error err -> Error err
reduceCBV _ = Error "invalid term"


-- call by name
reduceCBN :: Term -> ReduceResult
reduceCBN (Lam f) = Stop (Lam f)
reduceCBN (Name name) = Stop (Name name)
reduceCBN (App (Lam f) arg) = Step (f arg)
reduceCBN (App (Name f) arg) =
  case reduceCBN arg of
    Stop arg' -> Stop (App (Name f) arg')
    Step arg' -> Step (App (Name f) arg')
    Error err -> Error err
reduceCBN (App t1 t2) =
  case reduceCBN t1 of
    Stop t1' -> Step (App t1' t2)
    Step t1' -> Step (App t1' t2)
    Error err -> Error err
reduceCBN _ = Error "invalid term"



-- reduce iterator
reduceIter :: (Term -> ReduceResult) -> Int -> Term -> [Term]
reduceIter reducef d t = t : go d t where
  go 0 _ = []
  go d t = case reducef t of
    Step t' -> t' : go (d - 1) t'
    _ -> []

reduceIterInfi :: (Term -> ReduceResult) -> Term -> [Term]
reduceIterInfi reducef t = t : go t where
  go t = case reducef t of
    Step t' -> t' : go t'
    _ -> []

reduceIterInfiSteps :: (Term -> ReduceResult) -> Term -> (Term, Int)
reduceIterInfiSteps reducef t = go t 1 where
  go t steps = case reducef t of
    Step t' -> go t' (steps + 1)
    _ -> (t, steps)



-- first-order representations
data FOTerm
  =  FVar Int
  | FName String
  | FApp FOTerm FOTerm
  | FLam FOTerm
  deriving (Eq)

instance Show FOTerm where
  show (FVar n) = show n
  show (FApp t1 t2) = printf "(%s %s)" (show t1) (show t2)
  show (FLam body) = printf "λ.%s" (show body)
  show (FName name) = name

-- convert to first-order representation, with de Bruijn indices
toFO :: Term -> FOTerm
toFO (Var n) = FVar n
toFO (App t1 t2) = FApp (toFO t1) (toFO t2)
toFO (Lam f) = FLam (toFO (f (Var (depth (f (Var (-1)))))))
toFO (Name name) = FName name

depth :: Term -> Int
depth (Var _) = 0
depth (App t1 t2) = max (depth t1) (depth t2)
depth (Lam f) = 1 + depth (f (Var (-1)))
depth (Name _) = 0



instance Num Term where
  (+) = \x -> \y -> add @ x @ y
  (*) = \x -> \y -> mul @ x @ y
  (-) = error "Subtraction not defined in UTLC"
  negate = error "Negation not defined in UTLC"
  abs = error "Absolute value not defined in UTLC"
  signum = error "Signum not defined in UTLC"
  fromInteger n = nat (fromIntegral n)



-- allow string literals to be used as Name terms
instance IsString Term where
  fromString = Name


-- example terms

zro :: Term
zro = Lam $ \f -> Lam $ \z -> z

suc :: Term
suc = Lam $ \n -> Lam $ \f -> Lam $ \z -> f @ (n @ f @ z)

nat :: Int -> Term
nat 0 = zro
nat n | n > 0 = Lam $ \f -> Lam $ \z -> f @ (nat (n - 1) @ f @ z)
nat _ = error "nat: negative number"

tru :: Term
tru = Lam $ \x -> Lam $ \y -> x

fls :: Term
fls = Lam $ \x -> Lam $ \y -> y

ite :: Term
ite = Lam $ \p -> Lam $ \t -> Lam $ \f -> p @ t @ f

cst :: Term
cst = Lam $ \x -> Lam $ \y -> x

isz :: Term
isz = Lam $ \n -> n @ (cst @ fls) @ tru

ifz :: Term
ifz = Lam $ \n -> Lam $ \z -> Lam $ \nz -> n @ (cst @ nz) @ z

pair :: Term
pair = Lam $ \x -> Lam $ \y -> Lam $ \f -> f @ x @ y

fst_ :: Term
fst_ = Lam $ \p -> p @ (Lam $ \x -> Lam $ \y -> x)

snd_ :: Term
snd_ = Lam $ \p -> p @ (Lam $ \x -> Lam $ \y -> y)

prd :: Term
prd = Lam $ \n -> snd_ @ (n @ (Lam $ \p -> pair @ (suc @ (fst_ @ p)) @ (fst_ @ p) ) @ (pair @ zro @ zro))

add :: Term
add = Lam $ \m -> Lam $ \n -> Lam $ \f -> Lam $ \z -> m @ f @ (n @ f @ z)

mul :: Term
mul = Lam $ \m -> Lam $ \n -> Lam $ \f -> Lam $ \z -> m @ (n @ f) @ z

fct :: Term
fct = y @ (Lam $ \f -> Lam $ \n -> ifz @ n @ (nat 1) @ (mul @ n @ (f @ (prd @ n))))

self :: Term
self = Lam $ \x -> x @ x

omega :: Term
omega = self @ self

y :: Term
y = Lam $ \f -> (Lam $ \x -> f @ (x @ x)) @ (Lam $ \x -> f @ (x @ x))

mu :: Term
mu = Lam $ \t -> (y @ (Lam $ \n -> Lam $ \f -> ifz @ (t @ n) @ (f @ (suc @ n)))) @ zro
