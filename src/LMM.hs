{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module LMM where

import           Control.Monad
import           Control.Monad.Except
import           Data.List
import           Data.String
import           Text.Printf

-- |
-- * LMM - Lambda Mu Mu~
-- Higher and lower representation

-- |
-- ** `Core` language
-- Producers, Consumers and Statements

-- |
-- *** Producers
data Producer
  = -- | <μ> operator
    Pmu (Consumer -> Statement)
  | -- | <number> constant
    Pnum Int
  | -- | <data> data constructor
    Pcon Name [Producer] [Consumer]
  | -- | <codata> codata constructor
    Pcocase [(Name, Definition)]
  | -- | <helper> constructor for printing
    Pvar Int

-- |
-- *** Consumers
data Consumer
  = -- | <μ~> operator
    Cmu (Producer -> Statement)
  | -- | <global> consumer
    Cstar
  | -- | <codata> destructor
    Cdes Name [Producer] [Consumer]
  | -- | <data> destructor
    Ccase [(Name, Definition)]
  | -- | <helper> constructor for printing
    Cvar Int

-- |
-- *** Statements
data Statement
  = -- | pair of Producer and Consumer
    Spair Producer Consumer
  | -- | do real computation
    Sop (Int -> Int -> Int) Producer Producer Consumer
  | -- | judgement for zero
    Sifz Producer Statement Statement
  | -- | function calls
    Scall Name [Producer] [Consumer]

-- |
-- *** Top level definitions

-- | definitions
data Definition = Definition Int Int ([Producer] -> [Consumer] -> Statement)

-- | names
type Name = String

define :: Name -> Int -> Int -> ([Producer] -> [Consumer] -> Statement) -> (Name, Definition)
define name np nc f = (name, Definition np nc f)

-- | programs
type Program = [(Name, Definition)]

prettyP :: Int -> Producer -> String
prettyP d (Pmu f) = printf "μ(%s).%s" (prettyC d (Cvar d)) (prettyS (d + 1) (f (Cvar d)))
prettyP _ (Pnum n) = printf "[%d]" n
prettyP d (Pcon name ps ns) = printf "Kon{%s:%s;%s}" name (prettyPs d ps) (prettyCs d ns)
prettyP d (Pcocase defs) =
  printf "cocase{%s}" (intercalate "|" $ map (\(name, f) -> printf "%s" name (prettyDef d f)) defs)
prettyP _ (Pvar n) = printf "x%d" n

prettyPs :: Int -> [Producer] -> String
prettyPs d = intercalate "," . map (prettyP d)

prettyC :: Int -> Consumer -> String
prettyC d (Cmu f) = printf "μ~(%s).%s" (prettyP d (Pvar d)) (prettyS (d + 1) (f (Pvar d)))
prettyC _ Cstar = "{*}"
prettyC d (Cdes name ps ns) = printf "Des{%s:%s;%s}" name (prettyPs d ps) (prettyCs d ns)
prettyC d (Ccase defs) =
  printf "case{%s}" (intercalate "|" $ map (\(name, f) -> printf "%s" name (prettyDef d f)) defs)
prettyC _ (Cvar n) = printf "a%d" n

prettyCs :: Int -> [Consumer] -> String
prettyCs d = intercalate "," . map (prettyC d)

prettyDef :: Int -> Definition -> String
prettyDef d (Definition np nc f) =
  printf "[%s].[%s].%s" (prettyPs d vps) (prettyCs d vns) (prettyS (d + np + nc) (f vps vns))
    where
      vps = map Pvar [d .. d + np]
      vns = map Cvar [d + np .. d + np + nc]

prettyS :: Int -> Statement -> String
prettyS d (Spair p c) = printf "<%s|%s>" (prettyP d p) (prettyC d c)
prettyS d (Sop op p1 p2 c) = case op 1 1 of
  2 -> printf "op+(%s,%s;%s)" (prettyP d p1) (prettyP d p2) (prettyC d c)
  0 -> printf "op-(%s,%s;%s)" (prettyP d p1) (prettyP d p2) (prettyC d c)
  1 -> printf "op*(%s,%s;%s)" (prettyP d p1) (prettyP d p2) (prettyC d c)
  _ -> printf "op?(%s,%s;%s)" (prettyP d p1) (prettyP d p2) (prettyC d c)
prettyS d (Sifz p s1 s2) = printf "ifz(%s;%s,%s)" (prettyP d p) (prettyS (d + 1) s1) (prettyS (d + 1) s2)
prettyS d (Scall name ps cs) = printf "CALL(%s:%s;%s)" name (prettyPs d ps) (prettyCs d cs)

instance Show Producer where
  show = prettyP 0

instance Show Consumer where
  show = prettyC 0

instance Show Statement where
  show = prettyS 0



-- |
-- ** Reduction of `Core`

-- | reduce a statement
reduceStatement :: Program -> Statement -> Except String Statement
reduceStatement _ (Sop op (Pnum n1) (Pnum n2) c) = return (Spair (Pnum (op n1 n2)) c)
reduceStatement _ (Sop {}) = throwError "Cannot reduce statement with non-numeric producers"
reduceStatement _ (Sifz (Pnum 0) s1 _) = return s1
reduceStatement _ (Sifz (Pnum _) _ s2) = return s2
reduceStatement _ (Sifz {}) = throwError "Cannot reduce statement with non-numeric producer in ifz"
reduceStatement _ (Spair (Pmu f) c) = return (f c)
reduceStatement _ (Spair pv@(Pnum _) (Cmu f)) = return (f pv)
reduceStatement _ (Spair (Pcon name ps cs) (Ccase defs)) = do
  if all valueP ps && all valueN cs then do
    def <- withError (printf "Data: In %s " name ++) (reduceName name defs)
    reduceDefinition def ps cs
  else throwError "Data: Only reduce with values"
reduceStatement _ (Spair (Pcocase defs) (Cdes name ps cs)) = do
  if all valueP ps && all valueN cs then do
    def <- withError (printf "Codata: In %s " name ++) (reduceName name defs)
    reduceDefinition def ps cs
  else throwError "Codata: Only reduce with values"
reduceStatement _ (Spair _ Cstar) = throwError "Reduction stops with <*>"
reduceStatement _ (Spair {}) = throwError "Invalid reduction"
reduceStatement prog (Scall name ps cs) = do
  if all valueP ps && all valueN cs then do
    def <- withError ("Function: " ++) (reduceName name prog)
    reduceDefinition def ps cs
  else throwError "Function: Only call with values"

reduceName :: String -> [(Name, Definition)] -> Except String Definition
reduceName name defs = case lookup name defs of
  Just def -> return def
  Nothing  -> throwError $ printf "Binding: [%s] not defined" name

reduceDefinition :: Definition -> [Producer] -> [Consumer] -> Except String Statement
reduceDefinition (Definition np nc f) ps cs
  | np == length ps && nc == length cs = return (f ps cs)
  | otherwise = throwError "Binding: Wrong numbers of arguments"

valueP :: Producer -> Bool
valueP (Pnum _) = True
valueP _        = False

valueN :: Consumer -> Bool
valueN (Cmu _) = True
valueN Cstar   = True
valueN _       = False

-- | reduce a statement iteratively until no more reductions are possible
reduceIter :: Statement -> Except String [Statement]
reduceIter s = (s :) <$> go s
  where
    go s = (reduceStatement defined s >>= (\s' -> (s' :) <$> go s')) `catchError` const (return [])

-- | reduce a statement and print the list
reduceIterList :: Statement -> IO ()
reduceIterList s = do
  let ss = runExcept (reduceIter s)
  case ss of
    Left err         -> putStrLn $ "Error: " ++ err
    Right statements -> mapM_ (putStrLn . ("--> " ++) . show) statements

-- | reduce a statement interactively, waiting for user input after each reduction
reduceIterInteractive :: Statement -> IO ()
reduceIterInteractive s = do
  let ss = runExcept (reduceIter s)
  case ss of
    Left err         -> putStrLn $ "Error: " ++ err
    Right statements -> mapM_ ((putStrLn . ("--> " ++) . show) >=> const (void getLine)) statements



-- |
-- ** Example Constructs

-- |
-- *** The high level `Fun` language
-- Use higher order syntax to define high level language.
-- Translate it directly into `Core`

-- | terms in `Fun` are producers
type Fun = Producer

-- | if operator
fifz :: Fun -> Fun -> Fun -> Fun
fifz test then' else' = Pmu (\a -> Sifz test (Spair then' a) (Spair else' a))

fop :: (Int -> Int -> Int) -> Fun -> Fun -> Fun
fop op p1 p2 = Pmu (\a -> Sop op p1 p2 a)

flet :: Fun -> (Fun -> Fun) -> Fun
flet bind body = Pmu (\a -> Spair bind (Cmu (\x -> Spair (body x) a)))

fcall :: Name -> [Producer] -> [Consumer] -> Producer
fcall name ps cs = Pmu (\a -> Scall name ps (cs ++ [a]))

fdef :: Name -> Int -> Int -> ([Producer] -> [Consumer] -> Producer) -> (Name, Definition)
fdef name np nc def = (name, Definition np (nc + 1) \ps csa -> Spair (def ps (init csa)) (last csa))

instance Num Fun where
  (+) = fop (+)
  (-) = fop (-)
  (*) = fop (*)
  negate n = Pnum 0 - n
  abs = error "Not Supported: abs"
  signum = error "Not Supported: signum"
  fromInteger n = Pnum (fromInteger n)

-- |
-- *** Reduce `Fun`

-- | auto reduce
reduceFunList :: Producer -> IO ()
reduceFunList = reduceIterList . stop

-- | interactive reduce
reduceFunInteractive :: Producer -> IO ()
reduceFunInteractive = reduceIterInteractive . stop

-- |
-- *** `Fun` Examples
egf1 :: Fun
egf1 = flet 5 (\x -> 2 + x)

egf2 :: Fun
egf2 = flet (2 * 2) (\x -> x * x)

defined :: Program
defined =
  [ define "fct" 1 1
      \[x] [a] -> Sifz x (Spair 1 a) (sminus x 1 (Cmu \y -> Scall "fct" [y] [Cmu \z -> smul x z a]))
  , define "fib" 1 1
      \[x] [a] -> Sifz x (Spair 1 a) (sminus x 1 (Cmu \y -> Sifz y (Spair 1 a)
        (sminus x 1 (Cmu \z -> Scall "fib" [z] [Cmu \w ->
          sminus x 2 (Cmu \z1 -> Scall "fib" [z1] [Cmu \w1 ->
            splus w w1 a])]))))
  , fdef "m2" 1 0 \[x] [] -> x * 2
  , fdef "a2" 1 0 \[x] [] -> x + 2
  ]

instance IsString Producer where
  fromString s = case reads s of
    [(n, "")] -> Pnum n
    _         -> error $ "Cannot parse Producer from string: " ++ s



-- |
-- ** Low level macros for `Core`

-- | plus
splus :: Producer -> Producer -> Consumer -> Statement
splus = Sop (+)

-- | minus
sminus :: Producer -> Producer -> Consumer -> Statement
sminus = Sop (-)

-- | multiply
smul :: Producer -> Producer -> Consumer -> Statement
smul = Sop (*)

-- | top level
stop :: Producer -> Statement
stop p = Spair p Cstar

eg1 :: Statement
eg1 = stop $ Pmu (\a -> splus (Pnum 1) (Pnum 2) a)

-- ⟨μα.ifz(⌜2⌝, ⟨⌜5⌝ | α⟩, ⟨⌜10⌝ | α⟩) | ⋆⟩
eg2 :: Statement
eg2 = stop $ Pmu (\a -> Sifz (Pnum 2) (Spair (Pnum 5) a) (Spair (Pnum 10) a))

-- translate from (⌜2⌝ ∗ ⌜4⌝) + ⌜5⌝
eg3 :: Statement
eg3 = stop $ Pmu (\a -> splus (Pmu (\b -> smul (Pnum 2) (Pnum 4) b)) (Pnum 5) a)

eg4 :: Statement
eg4 = stop (Pmu (\a -> splus (Pnum 1) (Pnum 2) a))
