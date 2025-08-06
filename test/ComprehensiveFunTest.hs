module ComprehensiveFunTest (testComprehensiveFunConstructs) where

import           LMM

-- |
-- *** Comprehensive Fun Language Construct Test Functions
-- 
-- This module specifically tests the focusing behavior of all Fun language constructs

-- Comprehensive test: expressions using all Fun language constructs
testComprehensiveFunConstructs :: IO ()
testComprehensiveFunConstructs = do
  putStrLn "\n=== Comprehensive Fun Language Construct Focusing Tests ==="
  putStrLn "\nTesting all Fun language constructs:"
  putStrLn "- fifz, fop, flet, flam, @, fcon, fcase, fcocase, fdes, fcall, flab, fgoto"
  
  -- 1. Conditional control and arithmetic operations
  putStrLn "\n--- Conditional Control and Arithmetic Operations ---"
  let conditional_expr = 
        flet 5 $ \x ->
        fifz x (fop (+) x 100) (fop (*) x x)
  testExpressionFocusing "conditional arithmetic" conditional_expr
  
  -- 2. Higher-order functions
  putStrLn "\n--- Higher-Order Functions ---"
  let higher_order_expr = 
        let add_one = flam $ \x -> fop (+) x 1
        in add_one @ 5
  testExpressionFocusing "higher-order function" higher_order_expr
  
  -- 3. Data construction and matching
  putStrLn "\n--- Data Construction and Matching ---"
  let data_expr = 
        let list = fcon "Cons" [10, fcon "Nil" []]
        in fcase list [
          ("Nil", FBind 0 (\[] -> 0)),
          ("Cons", FBind 2 (\[h, _t] -> fop (+) h 100))
        ]
  testExpressionFocusing "data operations" data_expr
  
  -- 4. Codata
  putStrLn "\n--- Codata Operations ---"
  let codata_expr = 
        let stream = fcocase [("hd", FBind 0 (\[] -> 42))]
        in fdes stream "hd" []
  testExpressionFocusing "codata" codata_expr
  
  -- 5. Function calls
  putStrLn "\n--- Function Calls ---"
  let recursive_expr = 
        flet (fcall "fct" [4] []) $ \fact4 ->
        fop (+) fact4 100
  testExpressionFocusing "recursive call" recursive_expr
  
  -- 6. Control flow
  putStrLn "\n--- Control Flow and Labels ---"
  let control_expr = 
        flab $ \exit_label ->
          flet 3 $ \counter ->
          fifz counter (fgoto 999 exit_label) (fop (*) counter 10)
  testExpressionFocusing "control flow" control_expr
  
  -- 7. Composite expressions
  putStrLn "\n--- Composite Expressions ---"
  let complex_expr = 
        flet (flam $ \x -> fop (+) x 1) $ \inc ->
        flet (fcon "Just" [5]) $ \maybe_val ->
        fcase maybe_val [
          ("Nothing", FBind 0 (\[] -> 0)),
          ("Just", FBind 1 (\[val] -> inc @ val))
        ]
  testExpressionFocusing "composite expression" complex_expr

  putStrLn "\n=== Fun Language Construct Tests Complete ==="

-- Helper function: test focusing differences for a single expression
testExpressionFocusing :: String -> Fun -> IO ()
testExpressionFocusing name expr = do
  putStrLn $ "Test: " ++ name
  
  let protected_expr = focusing True expr
      unprotected_expr = focusing False expr
      protected_clean = unsyntax protected_expr
      are_same = show protected_clean == show unprotected_expr
      
  putStrLn $ "  Original: " ++ take 80 (show expr) ++ "..."
  putStrLn $ "  Protected: " ++ take 80 (show protected_expr) ++ "..."
  putStrLn $ "  Unprotected: " ++ take 80 (show unprotected_expr) ++ "..."
  putStrLn $ "  Same after removing marks: " ++ show are_same
  
  if not are_same then
    putStrLn "  *** Structural difference found! ***"
  else
    putStrLn "  Structure identical (normal)"
