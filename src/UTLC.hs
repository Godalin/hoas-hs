-- Everything you want about untyped lambda calculus
-- is in this file
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module UTLC where

reduceList :: Term -> IO ()
reduceList t = mapM_ print (reduceCBVIter 20 t)

data Term
  = App Term Term
  | Lam (Term -> Term)
  | Var Int -- for printing
  | Name String -- for named terms, not used in reduction

prettyTerm :: Int -> Term -> String
prettyTerm d (App t1 t2) = "(" ++ prettyTerm d t1 ++ " " ++ prettyTerm d t2 ++ ")"
prettyTerm d (Lam f) = "λ(x" ++ show d ++ ")." ++ prettyTerm (d + 1) (f (Var d))
prettyTerm _ (Var n) = "x" ++ show n
prettyTerm _ (Name name) = name

instance Show Term where
  show = prettyTerm 0


reduceCBV :: Term -> Either String Term
reduceCBV (App (Lam f) arg) = Right (f arg)
reduceCBV (Lam f) = Right (Lam f)
reduceCBV (Name name) = Right (Name name)
reduceCBV _ = Left "Cannot reduce"

reduceCBVIter :: Int -> Term -> [Term]
reduceCBVIter d t = t : go d t where
  go 0 _ = []
  go d t = case reduceCBV t of
    Right t' -> t' : go (d - 1) t'
    Left _ -> []



-- example terms

self :: Term
self = Lam (\x -> App x x)

omega :: Term
omega = App self self

y :: Term
y = Lam (\f -> App
      (Lam (\x -> App f (App x x)))
      (Lam (\x -> App f (App x x))))
