module ShowDifference where

import           Control.Monad.Except
import           LMM

-- Show the real difference between two focusing methods
-- Key point: don't use unsyntax!

showProtectionDifference :: IO ()
showProtectionDifference = do
  putStrLn "=== Show Real Difference of Syntax Protection ==="
  putStrLn ""

  -- Test expression: \x -> x + (1 + 2)
  let test_expr = flam (\x -> x + (1 + 2))

  putStrLn "Original expression:"
  putStrLn $ show test_expr
  putStrLn ""

  putStrLn "Protected focusing (with syntax protection):"
  let protected = focusing True test_expr
  putStrLn $ show protected
  putStrLn ""

  putStrLn "Unprotected focusing (without syntax protection):"
  let unprotected = focusing False test_expr
  putStrLn $ show unprotected
  putStrLn ""

  putStrLn "Protected focusing (after removing syntax marks):"
  let protected_clean = unsyntax protected
  putStrLn $ show protected_clean
  putStrLn ""

  putStrLn "Compare if they are the same:"
  putStrLn $ "Protected (clean) == Unprotected: " ++ show (show protected_clean == show unprotected)

-- Show internal structure of syntax variables
showSyntaxVariables :: IO ()
showSyntaxVariables = do
  putStrLn "=== Internal Structure of Syntax Variables ==="
  putStrLn ""

  -- During focusing process, bound variables should be handled differently
  let test_expr = flam (\x -> x + (1 + 2))

  -- Enable showVAR to display syntax variables
  putStrLn "After enabling syntax variable display:"
  putStrLn ""

  putStrLn "Protected focusing:"
  let protected = focusing True test_expr
  putStrLn $ show protected
  putStrLn ""

  putStrLn "Unprotected focusing:"
  let unprotected = focusing False test_expr
  putStrLn $ show unprotected
  putStrLn ""

-- Show more complex examples
demonstrateDeepFocusing :: IO ()
demonstrateDeepFocusing = do
  putStrLn "=== Deep Focusing Differences ==="
  putStrLn ""

  -- More complex nested expression
  let complex_expr = flam (\x ->
        flet (x + 1) (\y ->
          flet (y + 2) (\z ->
            z + (3 + 4))))

  putStrLn "Complex nested expression:"
  putStrLn $ show complex_expr
  putStrLn ""

  putStrLn "Protected focusing:"
  let protected = focusing True complex_expr
  putStrLn $ show protected
  putStrLn ""

  putStrLn "Unprotected focusing:"
  let unprotected = focusing False complex_expr
  putStrLn $ show unprotected
  putStrLn ""

  putStrLn $ "Are they the same: " ++ show (show protected == show unprotected)

-- |
-- *** Specifically test reduction differences (corrected version)

testReductionDifferences :: IO ()
testReductionDifferences = do
  putStrLn "=== Detailed Reduction Behavior Comparison (Corrected Version) ==="
  putStrLn ""

  -- Simple test expression: (\x -> x + (1 + 2)) @ 5
  let test_expr = (flam (\x -> x + (1 + 2))) @ 5

  putStrLn "Test expression: (\\x -> x + (1 + 2)) @ 5"
  putStrLn ""
  putStrLn "Important note: Protected focusing results must use unsyntax to remove syntax marks before reduction"
  putStrLn ""

  -- Original expression
  putStrLn "1. Original expression reduction:"
  putStrLn $ "   " ++ show (stop test_expr)
  putStrLn ""
  reduceIterList (stop test_expr)
  putStrLn ""

  -- Protected focusing (correctly remove marks)
  putStrLn "2. Protected focusing reduction (after removing syntax marks):"
  let protected_expr = unsyntax $ focusing True test_expr
  putStrLn $ "   " ++ show (stop protected_expr)
  putStrLn ""
  reduceIterList (stop protected_expr)
  putStrLn ""

  -- Unprotected focusing
  putStrLn "3. Unprotected focusing reduction:"
  let unprotected_expr = focusing False test_expr
  putStrLn $ "   " ++ show (stop unprotected_expr)
  putStrLn ""
  reduceIterList (stop unprotected_expr)
  putStrLn ""

  putStrLn "4. Reduction step count comparison:"
  let orig_steps = length $ either (const []) id $ runExcept $ reduceIter (stop test_expr)
      protected_steps = length $ either (const []) id $ runExcept $ reduceIter (stop protected_expr)
      unprotected_steps = length $ either (const []) id $ runExcept $ reduceIter (stop unprotected_expr)

  putStrLn $ "   Original expression reduction steps: " ++ show orig_steps
  putStrLn $ "   Protected focusing (unsyntax) reduction steps: " ++ show protected_steps
  putStrLn $ "   Unprotected focusing reduction steps: " ++ show unprotected_steps

  if protected_steps == unprotected_steps
    then putStrLn "   Result: Reduction steps are the same after removing syntax marks"
    else putStrLn "   Result: Reduction steps still differ even after removing syntax marks"
  putStrLn ""

-- |
-- *** Compare reduction differences across different complexity expressions (corrected version)

compareReductionComplexity :: IO ()
compareReductionComplexity = do
  putStrLn "=== Reduction Comparison for Different Complexity Expressions (Corrected Version) ==="
  putStrLn ""
  putStrLn "Note: Protected focusing results have been processed with unsyntax to remove syntax marks"
  putStrLn ""

  let test_cases =
        [ ("Simple", 5 + 3)
        , ("Nested operations", (1 + 2) + (3 + 4))
        , ("With variables", (flam (\x -> x + 1)) @ 5)
        , ("Complex expression", (flam (\x -> x + (1 + 2))) @ 5)
        , ("More complex", (flam (\x -> (x + 1) + (2 + 3))) @ 10)
        ]

  mapM_ analyzeCase test_cases
  where
    analyzeCase (name, expr) = do
      putStrLn $ "--- " ++ name ++ " ---"
      putStrLn $ "Expression: " ++ show expr

      let orig_steps = getStepCount (stop expr)
          protected_steps = getStepCount (stop $ unsyntax $ focusing True expr)  -- Key modification
          unprotected_steps = getStepCount (stop $ focusing False expr)

      putStrLn $ "Original reduction steps: " ++ show orig_steps
      putStrLn $ "Protected (unsyntax) reduction steps: " ++ show protected_steps
      putStrLn $ "Unprotected reduction steps: " ++ show unprotected_steps

      if protected_steps /= unprotected_steps
        then putStrLn "*** Found reduction step difference! ***"
        else putStrLn "Reduction steps are the same"
      putStrLn ""

    getStepCount stmt = length $ either (const []) id $ runExcept $ reduceIter stmt
