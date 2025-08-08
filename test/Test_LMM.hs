{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test_LMM where

import           Control.Monad
import           LMM

-- |
-- ** Comprehensive Core and Fun Language Test Module
--
-- This module provides focused testing of:
-- 1. Core language expressions with reduction sequences
-- 2. Fun language expressions with reduction sequences
-- 3. Configurable focusing protection display

-- |
-- *** Configuration

-- Global flag to control whether to show unprotected focusing results
-- Set to False to only show protected focusing (default)
-- Set to True to show both protected and unprotected focusing
showUnprotectedFocusing :: Bool
showUnprotectedFocusing = False

-- |
-- *** Test Program Definitions

-- Core language test program
coreTestProgram :: Program
coreTestProgram =
  [
    -- factorial in Core language
    define "fct" 1 1
      \[x] [a] -> Sifz x (Spair 1 a) (sminus x 1 (Cmu \y -> Scall "fct" [y] [Cmu \z -> smul x z a]))

    -- fibonacci in Core language
  , define "fib" 1 1
      \[x] [a] -> Sifz x (Spair 1 a) (sminus x 1 (Cmu \y -> Sifz y (Spair 1 a)
        (sminus x 1 (Cmu \z -> Scall "fib" [z] [Cmu \w ->
          sminus x 2 (Cmu \z1 -> Scall "fib" [z1] [Cmu \w1 ->
            splus w w1 a])]))))

    -- power function: pow(x,n) = x^n
  , define "pow" 2 1
      \[x, n] [a] -> Sifz n (Spair 1 a) (sminus n 1 (Cmu \n' -> Scall "pow" [x, n'] [Cmu \p -> smul x p a]))
  ]

-- Fun language test program (comprehensive)
funTestProgram :: Program
funTestProgram =
  [
    -- Basic arithmetic functions
    fdef' "inc" 1 0 \[x] [] -> x + 1
  , fdef' "dec" 1 0 \[x] [] -> x - 1
  , fdef' "double" 1 0 \[x] [] -> x * 2
  , fdef' "square" 1 0 \[x] [] -> x * x
  , fdef' "add" 2 0 \[x, y] [] -> x + y

    -- Advanced arithmetic functions
  , fdef' "fct" 1 0 \[n] [] -> fifz n 1 (n * fcall "fct" [n - 1] [])
  , fdef' "fib" 1 0 \[n] [] ->
      fifz n 1 (
        fifz (n - 1) 1 (
          fcall "fib" [n - 1] [] + fcall "fib" [n - 2] []
        )
      )
  , fdef' "akm" 2 0 \[m, n] [] ->
      fifz m (n + 1) (
        fifz n
          (fcall "akm" [m - 1, 1] [])
          (fcall "akm" [m - 1, fcall "akm" [m, n - 1] []] [])
      )

    -- Multiplication functions with early exit comparison
  , fdef' "mult" 1 0 \[l] [] -> flab (\alpha -> fcall "mult'" [l] [alpha])
  , fdef' "mult'" 1 1 \[l] [alpha] -> fcase l
      [ ("Nil", FBind 0 \[] -> 1)
      , ("Cons", FBind 2 \[x, xs] ->
            fifz x (fgoto 0 alpha) (x * fcall "mult'" [xs] [alpha]))
      ]
  , fdef' "mult_no_exit" 1 0 \[l] [] -> fcase l
      [ ("Nil", FBind 0 \[] -> 1)
      , ("Cons", FBind 2 \[x, xs] -> x * fcall "mult_no_exit" [xs] [])
      ]

    -- List operations
  , fdef' "head" 1 0 \[xs] [] -> fcase xs
      [ ("Nil", FBind 0 \[] -> 0)
      , ("Cons", FBind 2 \[x, _] -> x)
      ]
  , fdef' "tail" 1 0 \[xs] [] -> fcase xs
      [ ("Nil", FBind 0 \[] -> fcon "Nil" [])
      , ("Cons", FBind 2 \[_, xs'] -> xs')
      ]
  , fdef' "length" 1 0 \[xs] [] -> fcase xs
      [ ("Nil", FBind 0 \[] -> 0)
      , ("Cons", FBind 2 \[_, xs'] -> 1 + fcall "length" [xs'] [])
      ]
  , fdef' "sum" 1 0 \[xs] [] -> fcase xs
      [ ("Nil", FBind 0 \[] -> 0)
      , ("Cons", FBind 2 \[x, xs'] -> x + fcall "sum" [xs'] [])
      ]

    -- Pair operations
  , fdef' "fst" 1 0 \[p] [] -> fcase p
      [ ("Pair", FBind 2 \[x, _] -> x)
      ]
  , fdef' "snd" 1 0 \[p] [] -> fcase p
      [ ("Pair", FBind 2 \[_, y] -> y)
      ]
  , fdef' "swap" 1 0 \[p] [] -> fcase p
      [ ("Pair", FBind 2 \[x, y] -> fcon "Pair" [y, x])
      ]

    -- Lazy operations (codata)
  , fdef' "lazy_pair" 2 0 \[x, y] [] -> fcocase
      [ ("fst", FBind 0 \[] -> x)
      , ("snd", FBind 0 \[] -> y)
      ]

    -- Stream operations
  , fdef' "repeat" 1 0 \[x] [] -> fcocase
      [ ("hd", FBind 0 \[] -> x)
      , ("tl", FBind 0 \[] -> fcall "repeat" [x] [])
      ]
  , fdef' "nats_from" 1 0 \[n] [] -> fcocase
      [ ("hd", FBind 0 \[] -> n)
      , ("tl", FBind 0 \[] -> fcall "nats_from" [n + 1] [])
      ]

    -- Mutual recursion
  , fdef' "is_even" 1 0 \[x] [] -> fifz x (fcon "True" []) (fcall "is_odd" [x - 1] [])
  , fdef' "is_odd" 1 0 \[x] [] -> fifz x (fcon "False" []) (fcall "is_even" [x - 1] [])
  ]

-- |
-- *** Helper Functions

-- Print reduction sequence for Core language tests
printCoreReduction :: Statement -> IO ()
printCoreReduction = reduceIterList coreTestProgram

-- Print reduction sequence for Fun language tests
printFunReduction :: Fun -> IO ()
printFunReduction = reduceFunList funTestProgram

-- Create focused versions and print reduction sequences for Core tests
runCoreReductionTest :: String -> String -> Statement -> IO ()
runCoreReductionTest testNum description original = do
  putStrLn $ testNum ++ ". " ++ description
  putStrLn $ replicate (length (testNum ++ ". " ++ description)) '-'

  -- Original expression
  putStrLn "Original expression:"
  printCoreReduction original
  putStrLn ""

  -- Core language doesn't support focusing, so note this
  putStrLn "Note: Focusing not applicable to Core language constructs"
  putStrLn ""

-- Create focused versions and print reduction sequences for Fun tests
runFunReductionTest :: String -> String -> Fun -> IO ()
runFunReductionTest testNum description original = do
  putStrLn $ testNum ++ ". " ++ description
  putStrLn $ replicate (length (testNum ++ ". " ++ description)) '-'

  -- Original expression
  putStrLn "Original expression:"
  printFunReduction original
  putStrLn ""

  -- Protected focusing (always show) - only for Fun-based expressions
  putStrLn "Protected focusing:"
  let protected = unsyntax $ focusing True original
  printFunReduction protected
  putStrLn ""

  -- Unprotected focusing (conditional)
  when showUnprotectedFocusing $ do
    putStrLn "Unprotected focusing:"
    let unprotected = focusing False original
    printFunReduction unprotected
    putStrLn ""

-- |
-- *** Core Language Tests

runCoreLanguageTests :: IO ()
runCoreLanguageTests = do
  putStrLn "=== Core Language Tests ==="
  putStrLn ""

  -- Test 1: Simple arithmetic
  runCoreReductionTest "C1" "Simple arithmetic: 2 + 3"
    (Spair (2 + 3) Cstar)

  -- Test 2: Conditional with zero
  runCoreReductionTest "C2" "Conditional (zero): if 0 then 5 else 10"
    (Sifz 0 (Spair 5 Cstar) (Spair 10 Cstar))

  -- Test 3: Conditional with non-zero
  runCoreReductionTest "C3" "Conditional (non-zero): if 3 then 5 else 10"
    (Sifz 3 (Spair 5 Cstar) (Spair 10 Cstar))

  -- Test 4: Function call - factorial
  runCoreReductionTest "C4" "Function call: fct(3)"
    (Scall "fct" [3] [Cstar])

  -- Test 5: Function call - fibonacci
  runCoreReductionTest "C5" "Function call: fib(4)"
    (Scall "fib" [4] [Cstar])

  -- Test 6: Function call - power
  runCoreReductionTest "C6" "Function call: pow(2,3)"
    (Scall "pow" [2, 3] [Cstar])

  -- Test 7: Complex arithmetic
  runCoreReductionTest "C7" "Complex arithmetic: (2 * 3) + (4 - 1)"
    (Spair (Pmu (\a -> splus (Pmu (\b -> smul 2 3 b)) (Pmu (\c -> sminus 4 1 c)) a)) Cstar)

-- |
-- *** Fun Language Tests

runFunLanguageTests :: IO ()
runFunLanguageTests = do
  putStrLn "=== Fun Language Tests ==="
  putStrLn ""

  -- Basic constructs
  -- Test 1: Simple lambda application
  runFunReductionTest "F1" "Lambda application: (\\x -> x + 1) @ 5"
    ((flam (\x -> x + 1)) @ 5)

  -- Test 2: Let binding
  runFunReductionTest "F2" "Let binding: let x = 3 in x * x"
    (flet 3 (\x -> x * x))

  -- Test 3: Higher-order function
  runFunReductionTest "F3" "Higher-order: let f = (\\x -> x + 1) in f @ 10"
    (flet (flam (\x -> x + 1)) (\f -> f @ 10))

  -- Test 4: Conditional in Fun
  runFunReductionTest "F4" "Conditional: if 0 then 100 else 200"
    (fifz 0 100 200)

  -- Test 5: Function call in Fun
  runFunReductionTest "F5" "Function call: inc(7)"
    (fcall "inc" [7] [])

  -- Advanced arithmetic tests
  -- Test 6: Factorial in Fun
  runFunReductionTest "F6" "Factorial: fct(5)"
    (fcall "fct" [5] [])

  -- Test 7: Fibonacci in Fun
  runFunReductionTest "F7" "Fibonacci: fib(6)"
    (fcall "fib" [6] [])

  -- Test 8: Ackermann function
  runFunReductionTest "F8" "Ackermann: akm(2, 3)"
    (fcall "akm" [2, 3] [])

  -- Data construction and case matching
  -- Test 9: Data construction
  runFunReductionTest "F9" "Data construction: Just(42)"
    (fcon "Just" [42])

  -- Test 10: Case matching - Just
  runFunReductionTest "F10" "Case matching Just: case Just(5) of Nothing -> 0 | Just(x) -> x + 10"
    (fcase (fcon "Just" [5])
           [("Nothing", FBind 0 \[] -> 0),
            ("Just", FBind 1 \[x] -> x + 10)])

  -- Test 11: Case matching - Nothing
  runFunReductionTest "F11" "Case matching Nothing: case Nothing of Nothing -> 42 | Just(x) -> x"
    (fcase (fcon "Nothing" [])
           [("Nothing", FBind 0 \[] -> 42),
            ("Just", FBind 1 \[x] -> x)])

  -- List operations
  -- Test 12: List construction
  runFunReductionTest "F12" "List construction: Cons(1, Cons(2, Nil))"
    (fcon "Cons" [1, fcon "Cons" [2, fcon "Nil" []]])

  -- Test 13: List head
  runFunReductionTest "F13" "List head: head(Cons(42, Nil))"
    (fcall "head" [fcon "Cons" [42, fcon "Nil" []]] [])

  -- Test 14: List tail
  runFunReductionTest "F14" "List tail: tail(Cons(1, Cons(2, Nil)))"
    (fcall "tail" [fcon "Cons" [1, fcon "Cons" [2, fcon "Nil" []]]] [])

  -- Test 15: List length
  runFunReductionTest "F15" "List length: length(Cons(1, Cons(2, Cons(3, Nil))))"
    (fcall "length" [fcon "Cons" [1, fcon "Cons" [2, fcon "Cons" [3, fcon "Nil" []]]]] [])

  -- Test 16: List sum
  runFunReductionTest "F16" "List sum: sum(Cons(10, Cons(20, Cons(30, Nil))))"
    (fcall "sum" [fcon "Cons" [10, fcon "Cons" [20, fcon "Cons" [30, fcon "Nil" []]]]] [])

  -- Pair/Tuple operations
  -- Test 17: Pair construction and fst
  runFunReductionTest "F17" "Pair fst: fst(Pair(10, 20))"
    (fcall "fst" [fcon "Pair" [10, 20]] [])

  -- Test 18: Pair snd
  runFunReductionTest "F18" "Pair snd: snd(Pair(10, 20))"
    (fcall "snd" [fcon "Pair" [10, 20]] [])

  -- Test 19: Pair swap
  runFunReductionTest "F19" "Pair swap: swap(Pair(10, 20))"
    (fcall "swap" [fcon "Pair" [10, 20]] [])

  -- Codata/Lazy operations
  -- Test 20: Lazy pair construction and destructuring
  runFunReductionTest "F20" "Lazy pair fst: fst of lazy_pair(100, 200)"
    (fdes (fcall "lazy_pair" [100, 200] []) "fst" [])

  -- Test 21: Lazy pair snd
  runFunReductionTest "F21" "Lazy pair snd: snd of lazy_pair(100, 200)"
    (fdes (fcall "lazy_pair" [100, 200] []) "snd" [])

  -- Stream operations
  -- Test 22: Repeat stream head
  runFunReductionTest "F22" "Repeat stream head: hd of repeat(42)"
    (fdes (fcall "repeat" [42] []) "hd" [])

  -- Test 23: Repeat stream tail head
  runFunReductionTest "F23" "Repeat stream tail head: hd of tl of repeat(42)"
    (fdes (fdes (fcall "repeat" [42] []) "tl" []) "hd" [])

  -- Test 24: Natural numbers stream
  runFunReductionTest "F24" "Natural numbers: hd of nats_from(5)"
    (fdes (fcall "nats_from" [5] []) "hd" [])

  -- Test 25: Natural numbers stream tail
  runFunReductionTest "F25" "Natural numbers tail: hd of tl of nats_from(5)"
    (fdes (fdes (fcall "nats_from" [5] []) "tl" []) "hd" [])

  -- Mutual recursion
  -- Test 26: Even number test
  runFunReductionTest "F26" "Is even: is_even(4)"
    (fcall "is_even" [4] [])

  -- Test 27: Odd number test
  runFunReductionTest "F27" "Is odd: is_odd(5)"
    (fcall "is_odd" [5] [])

  -- Test 28: Even number test (odd input)
  runFunReductionTest "F28" "Is even (odd): is_even(3)"
    (fcall "is_even" [3] [])

  -- Advanced combinations
  -- Test 29: Nested lambda with let
  runFunReductionTest "F29" "Nested lambda: let f = (\\x -> \\y -> x + y) in f @ 3 @ 4"
    (flet (flam (\x -> flam (\y -> x + y))) (\f -> (f @ 3) @ 4))

  -- Test 30: Complex conditional with functions
  runFunReductionTest "F30" "Complex conditional: if (square(2) - 3) then inc(10) else double(5)"
    (fifz (fcall "square" [2] [] - 3) (fcall "inc" [10] []) (fcall "double" [5] []))

  -- Control operators
  -- Test 31: Label and goto
  runFunReductionTest "F31" "Label escape: label (\\k -> 1 + goto(k, 42))"
    (flab (\k -> 1 + fgoto 42 k))

  -- Test 32: Complex control flow
  runFunReductionTest "F32" "Complex control: label (\\k -> if 0 then goto(k, 100) else 200)"
    (flab (\k -> fifz 0 (fgoto 100 k) 200))

  -- Test 33: Nested case with pairs
  runFunReductionTest "F33" "Nested case: case Just(Pair(1,2)) of Nothing -> 0 | Just(p) -> fst(p)"
    (fcase (fcon "Just" [fcon "Pair" [1, 2]])
           [("Nothing", FBind 0 \[] -> 0),
            ("Just", FBind 1 \[p] -> fcall "fst" [p] [])])

  -- Multiplication functions with early exit comparison
  -- Test 34: Multiplication with early exit (contains zero)
  runFunReductionTest "F34" "Multiplication with early exit: mult([1,2,0,3,4])"
    (fcall "mult" [fcon "Cons" [1, fcon "Cons" [2, fcon "Cons" [0, fcon "Cons" [3, fcon "Cons" [4, fcon "Nil" []]]]]]] [])

  -- Test 35: Multiplication with early exit (no zero)
  runFunReductionTest "F35" "Multiplication with early exit: mult([1,2,3,4])"
    (fcall "mult" [fcon "Cons" [1, fcon "Cons" [2, fcon "Cons" [3, fcon "Cons" [4, fcon "Nil" []]]]]] [])

  -- Test 36: Multiplication without early exit (contains zero)
  runFunReductionTest "F36" "Multiplication without early exit: mult_no_exit([1,2,0,3,4])"
    (fcall "mult_no_exit" [fcon "Cons" [1, fcon "Cons" [2, fcon "Cons" [0, fcon "Cons" [3, fcon "Cons" [4, fcon "Nil" []]]]]]] [])

  -- Test 37: Multiplication without early exit (no zero)
  runFunReductionTest "F37" "Multiplication without early exit: mult_no_exit([1,2,3,4])"
    (fcall "mult_no_exit" [fcon "Cons" [1, fcon "Cons" [2, fcon "Cons" [3, fcon "Cons" [4, fcon "Nil" []]]]]] [])

-- |
-- *** Main Test Interface

runAllLanguageTests :: IO ()
runAllLanguageTests = do
  putStrLn "Running Comprehensive Core and Fun Language Tests"
  putStrLn "================================================="
  putStrLn ""
  if showUnprotectedFocusing
    then putStrLn "Mode: Showing both protected and unprotected focusing"
    else putStrLn "Mode: Showing only protected focusing (default)"
  putStrLn ""

  runCoreLanguageTests
  putStrLn ""
  runFunLanguageTests
  putStrLn ""
  putStrLn "=== All Tests Complete ==="
