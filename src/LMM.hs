{-# OPTIONS_GHC -Wno-name-shadowing #-}
module LMM where

reduceList :: Statement -> IO ()
reduceList s = mapM_ print (reduceIter s)

data Producer
  = Pmu (Consumer -> Statement)
  | Pnum Int

data Consumer
  = Cstar
  | Cvar Int

data Statement
  = Spair Producer Consumer
  | Sop (Int -> Int -> Int) Producer Producer Consumer
  | Sifz Producer Statement Statement

prettyProducer :: Int -> Producer -> String
prettyProducer d (Pmu f) = "μ(" ++ (prettyConsumer (Cvar d)) ++ ")." ++ prettyStatement (d + 1) (f (Cvar d))
prettyProducer _ (Pnum n) = show n

prettyConsumer :: Consumer -> String
prettyConsumer Cstar = "*"
prettyConsumer (Cvar n) = "a" ++ show n

prettyStatement :: Int -> Statement -> String
prettyStatement d (Spair p c) = "<" ++ prettyProducer d p ++ "|" ++ prettyConsumer c ++ ">"
prettyStatement d (Sop op p1 p2 c) = case op 1 1 of
  2 -> "op+(" ++ prettyProducer d p1 ++ "," ++ prettyProducer d p2 ++ ";" ++ prettyConsumer c ++ ")"
  0 -> "op-(" ++ prettyProducer d p1 ++ "," ++ prettyProducer d p2 ++ ";" ++ prettyConsumer c ++ ")"
  1 -> "op*(" ++ prettyProducer d p1 ++ "," ++ prettyProducer d p2 ++ ";" ++ prettyConsumer c ++ ")"
  _ -> "knop(" ++ prettyProducer d p1 ++ "," ++ prettyProducer d p2 ++ ";" ++ prettyConsumer c ++ ")"
prettyStatement d (Sifz p s1 s2) = "ifz(" ++ prettyProducer d p ++ ";" ++ prettyStatement (d + 1) s1 ++ ";" ++ prettyStatement (d + 1) s2 ++ ")"

instance Show Producer where
  show p = prettyProducer 0 p

instance Show Consumer where
  show c = prettyConsumer c
  
instance Show Statement where
  show s = prettyStatement 0 s

reduceStatement :: Statement -> Either String Statement
reduceStatement (Sop op (Pnum n1) (Pnum n2) c) = Right (Spair (Pnum (op n1 n2)) c)
reduceStatement (Sifz (Pnum 0) s1 _) = Right s1
reduceStatement (Sifz (Pnum _) _ s2) = Right s2
reduceStatement (Spair (Pmu f) c) = Right (f c)
reduceStatement _ = Left "Cannot reduce statement"

reduceIter :: Statement -> [Statement]
reduceIter s = s : go s where
  go s = case reduceStatement s of
    Right s' -> s' : go s'
    Left _ -> []

splus :: Producer -> Producer -> Consumer -> Statement
splus = Sop (+)

sminus :: Producer -> Producer -> Consumer -> Statement
sminus = Sop (-)

smul :: Producer -> Producer -> Consumer -> Statement
smul = Sop (*)

eg1 :: Statement
eg1 = Spair (Pmu (\a -> splus (Pnum 1) (Pnum 2) a)) Cstar

-- ⟨μα .ifz(⌜2⌝, ⟨⌜5⌝ | α⟩, ⟨⌜10⌝ | α⟩) | ⋆⟩
eg2 :: Statement
eg2 = Spair (Pmu (\a -> Sifz (Pnum 2) (Spair (Pnum 5) a) (Spair (Pnum 10) a))) Cstar

-- translate from (⌜2⌝ ∗ ⌜4⌝) + ⌜5⌝
eg3 :: Statement
eg3 = Spair 
  (Pmu (\a -> splus
    (Pmu (\b -> smul (Pnum 2) (Pnum 4) b))
    (Pnum 5)
    a))
  Cstar