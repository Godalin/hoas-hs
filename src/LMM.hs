{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE ViewPatterns         #-}

module LMM where

import           Control.Monad
import           Control.Monad.Except
import           Data.List
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
  | -- | <helper> syntactical variable
    -- only for syntactical transformations
    Pstx Producer

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
  | -- | <helper> syntactical variable
    -- only for syntactical transformations
    Cstx Consumer

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
prettyP d (Pcon name ps ns) = printf "𝕂{%s:%s;%s}" name (prettyPs d ps) (prettyCs d ns)
prettyP d (Pcocase defs) =
  printf "cocase{%s}" (intercalate "|" $ map (\(name, f) -> name ++ prettyDef d f) defs)
prettyP _ (Pvar n) = printf "x%d" n
prettyP d (Pstx p) = printf "PVAR{%s}" $ prettyP d p

prettyPs :: Int -> [Producer] -> String
prettyPs d = intercalate "," . map (prettyP d)

prettyC :: Int -> Consumer -> String
prettyC d (Cmu f) = printf "̃μ(%s).%s" (prettyP d (Pvar d)) (prettyS (d + 1) (f (Pvar d)))
prettyC _ Cstar = "⋆"
prettyC d (Cdes name ps ns) = printf "𝔻{%s:%s;%s}" name (prettyPs d ps) (prettyCs d ns)
prettyC d (Ccase defs) =
  printf "case{%s}" (intercalate "|" $ map (\(name, f) -> name ++ prettyDef d f) defs)
prettyC _ (Cvar n) = printf "α%d" n
prettyC d (Cstx c) = printf "CVAR{%s}" $ prettyC d c

prettyCs :: Int -> [Consumer] -> String
prettyCs d = intercalate "," . map (prettyC d)

prettyDef :: Int -> Definition -> String
prettyDef d (Definition np nc f) =
  printf "[%s;%s].%s" (prettyPs d vps) (prettyCs d vns) (prettyS (d + np + nc) (f vps vns))
    where
      vps = map Pvar [d .. d + np - 1]
      vns = map Cvar [d + np .. d + np + nc - 1]

prettyS :: Int -> Statement -> String
prettyS d (Spair p c) = printf "⟨%s|%s⟩" (prettyP d p) (prettyC d c)
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
reduceStatement _ (unsyntax -> Sop op (Pnum n1) (Pnum n2) c) = return (Spair (Pnum (op n1 n2)) c)
reduceStatement _ (unsyntax -> Sop {}) = throwError "Cannot reduce statement with non-numeric producers"
reduceStatement _ (unsyntax -> Sifz (Pnum 0) s1 _) = return s1
reduceStatement _ (unsyntax -> Sifz (Pnum _) _ s2) = return s2
reduceStatement _ (unsyntax -> Sifz {}) = throwError "Cannot reduce statement with non-numeric producer in ifz"
reduceStatement _ (unsyntax -> Spair (Pmu f) c) = return (f c)
reduceStatement _ (unsyntax -> Spair pv@(Pnum _) (Cmu f)) = return (f pv)
reduceStatement _ (unsyntax -> Spair (Pcon name ps cs) (Ccase defs)) = do
  if all valueP ps && all valueN cs then do
    def <- withError (printf "Data: In %s " name ++) (reduceName name defs)
    reduceDefinition def ps cs
  else throwError "Data: Only reduce with values"
reduceStatement _ (unsyntax -> Spair (Pcocase defs) (Cdes name ps cs)) = do
  if all valueP ps && all valueN cs then do
    def <- withError (printf "Codata: In %s " name ++) (reduceName name defs)
    reduceDefinition def ps cs
  else throwError "Codata: Only reduce with values"
reduceStatement _ (unsyntax -> Spair _ Cstar) = throwError "Reduction stops with <*>"
reduceStatement _ (unsyntax -> Spair {}) = throwError "Invalid reduction"
reduceStatement prog (unsyntax -> Scall name ps cs) = do
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

-- | cancel the syntactical variables
class Unsyntax a where
  unsyntax :: a -> a

instance Unsyntax Producer where
  unsyntax (Pstx p) = p
  unsyntax p        = p

instance Unsyntax Consumer where
  unsyntax (Cstx c) = c
  unsyntax c        = c

instance Unsyntax Statement where
  unsyntax (Spair p c)        = Spair (unsyntax p) (unsyntax c)
  unsyntax (Sop f p1 p2 c)    = Sop f (unsyntax p1) (unsyntax p2) (unsyntax c)
  unsyntax (Sifz p s1 s2)     = Sifz (unsyntax p) (unsyntax s1) (unsyntax s2)
  unsyntax (Scall name ps cs) = Scall name (map unsyntax ps) (map unsyntax cs)

-- | producer values
valueP :: Producer -> Bool
valueP (Pnum _)      = True
valueP (Pcon _ ps _) = all valueP ps
valueP (Pcocase {})  = True
valueP (Pvar _)      = True
valueP (Pstx _)      = True
valueP _             = False

-- | consumer values (everything)
valueN :: Consumer -> Bool
valueN (Cmu _)    = True
valueN Cstar      = True
valueN (Cdes {})  = True
valueN (Ccase {}) = True
valueN (Cvar _)   = True
valueN (Cstx _)   = True
-- valueN _          = False

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
-- *** Focusing

class Focusing a where
  focusing :: a -> a

instance Focusing Producer where
  focusing p@(Pvar _)        = p
  focusing p@(Pnum _)        = p
  focusing p@(Pcocase _)     = p
  focusing (Pmu bind)        = Pmu \a -> focusing (bind (Cstx a))
  focusing (Pcon name ps cs) =
    let (vps, nvps) = span valueP ps in
    case nvps of
      []         -> Pcon name (map focusing vps) (map focusing cs)
      nvp : nvps -> Pmu \a -> Spair (focusing nvp) (Cmu \x -> Spair (Pcon name (vps ++ Pstx x : nvps) cs) (Cstx a))
  focusing (Pstx p) = Pstx p

instance Focusing Consumer where
  focusing c@(Cvar _)        = c
  focusing c@(Ccase _)       = c
  focusing Cstar             = Cstar
  focusing (Cmu bind)        = Cmu \x -> focusing (bind (Pstx x))
  focusing (Cdes name ps cs) =
    let (vps, nvps) = span valueP ps in
    case nvps of
      []         -> Cdes name (map focusing vps) (map focusing cs)
      nvp : nvps -> Cmu \y -> Spair (focusing nvp)
                                (Cmu \x -> Spair (Pstx y)
                                (Cdes name (vps ++ Pstx x : nvps) cs))
  focusing (Cstx c) = Cstx c

instance Focusing Statement where
  focusing (Spair p c) = Spair (focusing p) (focusing c)
  focusing (Sop f p1 p2 c)
    | not (valueP p1) = Spair p1 (Cmu \x -> focusing (Sop f (Pstx x) p2 c))
    | not (valueP p2) = Spair p2 (Cmu \x -> focusing (Sop f p1 (Pstx x) c))
    | otherwise = Sop f (focusing p1) (focusing p2) (focusing c)
  focusing (Sifz p s1 s2)
    | not (valueP p) = Spair (focusing p) (Cmu \x -> Sifz (Pstx x) s1 s2)
    | otherwise = Sifz (focusing p) (focusing s1) (focusing s2)
  focusing (Scall name ps cs) =
    let (vps, nvps) = span valueP ps in
    case nvps of
      []         -> Scall name (map focusing vps) (map focusing cs)
      nvp : nvps -> Spair nvp (Cmu \x -> Scall name (vps ++ Pstx x : nvps) cs)



-- |
-- ** Example Constructs

-- |
-- *** The high level `Fun` language
-- Use higher order syntax to define high level language.
-- Translate it directly into `Core`

-- | terms in `Fun` are producers
type Fun = Producer

data FDef = FDef Int Int ([Producer] -> [Consumer] -> Producer)

-- | if operator
fifz :: Fun -> Fun -> Fun -> Fun
fifz test then' else' = Pmu (\a -> Sifz test (Spair then' a) (Spair else' a))

fop :: (Int -> Int -> Int) -> Fun -> Fun -> Fun
fop op p1 p2 = Pmu (\a -> Sop op p1 p2 a)

flet :: Fun -> (Fun -> Fun) -> Fun
flet bind body = Pmu (\a -> Spair bind (Cmu (\x -> Spair (body x) a)))

fcall :: Name -> [Producer] -> [Consumer] -> Fun
fcall name ps cs = Pmu (\a -> Scall name ps (cs ++ [a]))

fdef :: Name -> FDef -> (Name, Definition)
fdef name (FDef np nc def) = (name, Definition np (nc + 1) \ps csa -> Spair (def ps (init csa)) (last csa))

fdef' :: Name -> Int -> Int -> ([Producer] -> [Consumer] -> Producer) -> (Name, Definition)
fdef' name np nc def = fdef name (FDef np nc def)

-- |
-- *** Data and Codata

fcon :: Name -> [Fun] -> Fun
fcon name ps = Pcon name ps []

fcase :: Fun -> [(Name, FBind)] -> Fun
fcase p fbinds = Pmu \a -> Spair p (Ccase $
  map (\(name, FDef np nc def) ->
    (name, Definition np nc (\ps cs -> Spair (def ps cs) a))) (fmap (fmap fbind2def) fbinds))

fdes :: Fun -> Name -> [Fun] -> Fun
fdes p name ps = Pmu \a -> Spair p (Cdes name ps [a])

fcocase :: [(Name, FBind)] -> Fun
fcocase fbinds = Pcocase $ map (uncurry fdef) (fmap (fmap fbind2def) fbinds)

data FBind = FBind Int ([Fun] -> Fun)

fbind2def :: FBind -> FDef
fbind2def (FBind n f) = FDef n 0 \ps _ -> f ps

-- |
-- higher order functions

-- | abstraction
flam :: (Fun -> Fun) -> Fun
flam f = Pcocase [("@", Definition 1 1 \[x] [a] -> Spair (f x) a)]

-- | application
fapp :: Fun -> Fun -> Fun
fapp t1 t2 = Pmu \a -> Spair t1 (Cdes "@" [t2] [a])

(@) :: Fun -> Fun -> Fun
(@) = fapp

-- |
-- control operators

type Lab = Consumer

-- | labels
flab :: (Lab -> Fun) -> Fun
flab bind = Pmu \a -> Spair (bind a) a

fgoto :: Fun -> Lab -> Fun
fgoto t a = Pmu \_ -> Spair t a

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
  [
    -- factorial in `Core`
    define "fct" 1 1
      \[x] [a] -> Sifz x (Spair 1 a) (sminus x 1 (Cmu \y -> Scall "fct" [y] [Cmu \z -> smul x z a]))

    -- fibonacci in `Core`
  , define "fib" 1 1
      \[x] [a] -> Sifz x (Spair 1 a) (sminus x 1 (Cmu \y -> Sifz y (Spair 1 a)
        (sminus x 1 (Cmu \z -> Scall "fib" [z] [Cmu \w ->
          sminus x 2 (Cmu \z1 -> Scall "fib" [z1] [Cmu \w1 ->
            splus w w1 a])]))))

    -- definitions in `Fun`
  , fdef' "main" 0 0 \[] [] -> 1 + (1 - 1)
  , fdef' "m2" 1 0 \[x] [] -> x * 2
  , fdef' "a2" 1 0 \[x] [] -> x + 2
  , fdef' "swap" 1 0 \[x] [] -> fcase x
    [ ("Tup", FBind 2 \[x, y] -> fcon "Tup" [y, x])
    ]
  , fdef' "tup_lazy" 2 0 \[x, y] [] -> fcocase
    [ ("fst", FBind 0 \[] -> x)
    , ("snd", FBind 0 \[] -> y)
    ]
  , fdef' "swap_lazy" 1 0 \[x] [] -> fcocase
    [ ("fst", FBind 0 \[] -> fdes x "snd" [])
    , ("snd", FBind 0 \[] -> fdes x "fst" [])
    ]
  , fdef' "sum" 1 0 \[xs] [] -> fcase xs
    [ ("Nil", FBind 0 \[] -> 0)
    , ("Cons", FBind 2 \[y, ys] -> y + fcall "sum" [ys] [])
    ]
  , fdef' "repeat" 1 0 \[x] [] -> fcocase [
      ("hd", FBind 0 \[] -> x),
      ("tl", FBind 0 \[] -> fcall "repeat" [x] [])
    ]
  ]



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
