{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module LMM where

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans
import Text.Printf

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
prettyProducer d (Pmu f) = "μ(" ++ prettyConsumer (Cvar d) ++ ")." ++ prettyStatement (d + 1) (f (Cvar d))
prettyProducer _ (Pnum n) = show n

prettyConsumer :: Consumer -> String
prettyConsumer Cstar = "*"
prettyConsumer (Cvar n) = "a" ++ show n

prettyStatement :: Int -> Statement -> String
prettyStatement d (Spair p c) = printf "<%s|%s>" (prettyProducer d p) (prettyConsumer c)
prettyStatement d (Sop op p1 p2 c) = case op 1 1 of
  2 -> printf "op+(%s,%s;%s)" (prettyProducer d p1) (prettyProducer d p2) (prettyConsumer c)
  0 -> printf "op-(%s,%s;%s)" (prettyProducer d p1) (prettyProducer d p2) (prettyConsumer c)
  1 -> printf "op*(%s,%s;%s)" (prettyProducer d p1) (prettyProducer d p2) (prettyConsumer c)
  _ -> printf "op?(%s,%s;%s)" (prettyProducer d p1) (prettyProducer d p2) (prettyConsumer c)
prettyStatement d (Sifz p s1 s2) = printf "ifz(%s;%s;%s)" (prettyProducer d p) (prettyStatement (d + 1) s1) (prettyStatement (d + 1) s2)

instance Show Producer where
  show = prettyProducer 0

instance Show Consumer where
  show = prettyConsumer

instance Show Statement where
  show = prettyStatement 0

reduceStatement :: Statement -> Except String Statement
reduceStatement (Sop op (Pnum n1) (Pnum n2) c) = return (Spair (Pnum (op n1 n2)) c)
reduceStatement (Sifz (Pnum 0) s1 _) = return s1
reduceStatement (Sifz (Pnum _) _ s2) = return s2
reduceStatement (Spair (Pmu f) c) = return (f c)
reduceStatement _ = throwError "Cannot reduce statement"

reduceIter :: Statement -> Except String [Statement]
reduceIter s = (s :) <$> go s
  where
    go s = (reduceStatement s >>= (\s' -> (s' :) <$> go s')) `catchError` const (return [])

reduceIterPrinter :: Statement -> IO ()
reduceIterPrinter s = do
  let ss = runExcept (reduceIter s)
  case ss of
    Left err -> putStrLn $ "Error: " ++ err
    Right statements -> mapM_ print statements

-- Example Constructs

splus :: Producer -> Producer -> Consumer -> Statement
splus = Sop (+)

sminus :: Producer -> Producer -> Consumer -> Statement
sminus = Sop (-)

smul :: Producer -> Producer -> Consumer -> Statement
smul = Sop (*)

stop :: Producer -> Statement
stop p = Spair p Cstar

eg1 :: Statement
eg1 = stop $ Pmu (\a -> splus (Pnum 1) (Pnum 2) a)

-- ⟨μα .ifz(⌜2⌝, ⟨⌜5⌝ | α⟩, ⟨⌜10⌝ | α⟩) | ⋆⟩
eg2 :: Statement
eg2 = stop $ Pmu (\a -> Sifz (Pnum 2) (Spair (Pnum 5) a) (Spair (Pnum 10) a))

-- translate from (⌜2⌝ ∗ ⌜4⌝) + ⌜5⌝
eg3 :: Statement
eg3 =
  stop $
    Pmu
      ( \a ->
          splus
            (Pmu (\b -> smul (Pnum 2) (Pnum 4) b))
            (Pnum 5)
            a
      )
