{-# LANGUAGE BlockArguments #-}
module TestFocusing where

import           LMM

-- |
-- ** Test Cases: Comparing Two Focusing Implementations
--
-- This module demonstrates the difference between:
-- 1. Protected focusing (True):  Uses Pstx/Cstx wrappers to protect bound variables
-- 2. Unprotected focusing (False): Direct variable passing without protection

-- |
-- *** Generic Helper Functions

-- Create focused versions of a Fun expression
makeFocusedVersions :: String -> Fun -> (Fun, Fun, Fun)
makeFocusedVersions _name original =
  ( original
  , focusing True original   -- protected
  , focusing False original  -- unprotected
  )

-- Create focused versions of a Statement by applying focusing to the whole expression
makeFocusedStatements :: String -> Fun -> (Statement, Statement, Statement)
makeFocusedStatements _name expr =
  ( stop expr                          -- original
  , stop $ focusing True expr          -- protected
  , stop $ focusing False expr         -- unprotected
  )

-- Display comparison between protected and unprotected focusing
showFocusingDiff :: (Show a) => String -> a -> a -> a -> IO ()
showFocusingDiff name original protected unprotected = do
  putStrLn $ "=== " ++ name ++ " ==="
  putStrLn $ "Original:     " ++ show original
  putStrLn $ "Protected:    " ++ show protected
  putStrLn $ "Unprotected:  " ++ show unprotected
  putStrLn ""

-- Generic function to run syntax tests
runTestCase :: (Show a) => String -> (a, a, a) -> IO ()
runTestCase name (original, protected, unprotected) =
  showFocusingDiff name original protected unprotected

-- Generic function to run reduction tests
runReductionTest :: String -> String -> (Statement, Statement, Statement) -> IO ()
runReductionTest testNum description (original, protected, unprotected) = do
  putStrLn $ testNum ++ ". " ++ description
  putStrLn $ replicate (length (testNum ++ ". " ++ description)) '-'
  putStrLn "Original expression:"
  reduceIterList original
  putStrLn ""
  putStrLn "Protected focusing:"
  reduceIterList protected
  putStrLn ""
  putStrLn "Unprotected focusing:"
  reduceIterList unprotected
  putStrLn ""

-- |
-- *** Test Case 1: Simple Lambda Abstraction
-- Shows what happens when focusing is applied to a lambda abstraction

-- Test definition and runner combined
testSimpleLambda :: IO ()
testSimpleLambda = do
  -- A simple lambda: \x -> x + 1
  let test_expr = flam (\x -> x + 1)
      versions = makeFocusedVersions "Simple Lambda" test_expr
  runTestCase "Simple Lambda" versions

-- |
-- *** Test Case 2: Nested Lambda Abstraction
-- Shows how nested lambdas are handled differently

testNestedLambda :: IO ()
testNestedLambda = do
  -- \f -> \x -> f (x + 1)
  let test_expr = flam (\f -> flam (\x -> f @ (x + 1)))
      versions = makeFocusedVersions "Nested Lambda" test_expr
  runTestCase "Nested Lambda" versions

-- |
-- *** Test Case 3: Lambda with Complex Expression
-- Bound variable x appears in an expression that requires focusing

testComplexLambda :: IO ()
testComplexLambda = do
  -- \x -> (x + 1) + (2 * 3)
  let test_expr = flam (\x -> (x + 1) + (2 * 3))
      versions = makeFocusedVersions "Complex Lambda" test_expr
  runTestCase "Complex Lambda" versions

-- |
-- *** Test Case 4: Let Binding
-- Shows the issue with let bindings

testLetLambda :: IO ()
testLetLambda = do
  -- let y = 2 + 3 in \x -> x + y
  let test_expr = flet (2 + 3) (\y -> flam (\x -> x + y))
      versions = makeFocusedVersions "Let Lambda" test_expr
  runTestCase "Let Lambda" versions

-- |
-- *** Test Case 5: Self Application
-- Shows variables used in application context

testSelfApplication :: IO ()
testSelfApplication = do
  -- \x -> x @ x (similar to \x -> x x)
  let test_expr = flam (\x -> x @ x)
      versions = makeFocusedVersions "Self Application" test_expr
  runTestCase "Self Application" versions

-- |
-- *** Test Case 6: Complex Control Flow
-- Including if statements with variables

testComplexControl :: IO ()
testComplexControl = do
  -- \x -> if (x + 1) then (2 * 3) else (4 + 5)
  let test_expr = flam (\x -> fifz (x + 1) (2 * 3) (4 + 5))
      versions = makeFocusedVersions "Complex Control" test_expr
  runTestCase "Complex Control" versions

-- |
-- *** Test Case 7: Critical Test Case - Variable in Non-Value Expression
-- This example best demonstrates the difference between the two implementations

testVariableInRedex :: IO ()
testVariableInRedex = do
  -- \x -> x + (1 + 2)  -- x is a variable, but (1 + 2) needs evaluation
  let test_expr = flam (\x -> x + (1 + 2))
      versions = makeFocusedVersions "Variable in Redex" test_expr
  runTestCase "Variable in Redex" versions

-- |
-- *** Test Case 8: More Complex Scenario
-- \x -> (x + 1) + (2 * 3) -- Both additions need evaluation, but x is a bound variable

testBoundVarInComplexExpr :: IO ()
testBoundVarInComplexExpr = do
  let test_expr = flam (\x -> (x + 1) + (2 * 3))
      versions = makeFocusedVersions "Bound Var in Complex Expr" test_expr
  runTestCase "Bound Var in Complex Expr" versions

-- |
-- *** Reduction Test Case 1: Simple Application
-- Shows different reduction behavior

testSimpleApplicationReduction :: IO ()
testSimpleApplicationReduction = do
  -- (\x -> x + (1 + 2)) @ 5
  let test_expr = (flam (\x -> x + (1 + 2))) @ 5
      versions = makeFocusedStatements "Simple Application" test_expr
  runReductionTest "1" "Simple Application Test: (\\x -> x + (1 + 2)) @ 5" versions

-- |
-- *** Reduction Test Case 2: Complex Let Binding

testComplexReduction :: IO ()
testComplexReduction = do
  -- let f = (\x -> x + x) in f (1 + 2)
  let test_expr = flet (flam (\x -> x + x)) (\f -> f @ (1 + 2))
      versions = makeFocusedStatements "Complex Let Binding" test_expr
  runReductionTest "2" "Complex Let Binding: let f = (\\x -> x + x) in f (1 + 2)" versions

-- |
-- *** Reduction Test Case 3: Nested Let

testNestedLetReduction :: IO ()
testNestedLetReduction = do
  -- let x = 5 in let f = (\y -> x + y) in f (1 + 2)
  let test_expr = flet 5 (\x ->
        flet (flam (\y -> x + y)) (\f ->
          f @ (1 + 2)))
      versions = makeFocusedStatements "Nested Let" test_expr
  runReductionTest "3" "Nested Let: let x = 5 in let f = (\\y -> x + y) in f (1 + 2)" versions

-- |
-- *** Reduction Test Case 4: Higher-Order Function

testHigherOrderReduction :: IO ()
testHigherOrderReduction = do
  -- (\f -> \x -> f (f x)) @ (\y -> y + 1) @ 5
  let test_expr = (flam (\f -> flam (\x -> f @ (f @ x)))) @ (flam (\y -> y + 1)) @ 5
      versions = makeFocusedStatements "Higher-Order Function" test_expr
  runReductionTest "4" "Higher-Order Function: (\\f -> \\x -> f (f x)) @ (\\y -> y + 1) @ 5" versions

-- |
-- *** Combined Test Runners

-- Run all basic syntax tests
runAllSyntaxTests :: IO ()
runAllSyntaxTests = do
  putStrLn "Testing Focusing Implementation Differences"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn "Protected focusing (True):  Uses Pstx/Cstx to protect bound variables"
  putStrLn "Unprotected focusing (False): Direct variable passing without protection"
  putStrLn ""

  testSimpleLambda
  testNestedLambda
  testComplexLambda
  testLetLambda
  testSelfApplication
  testComplexControl
  testVariableInRedex
  testBoundVarInComplexExpr

-- Run all reduction tests
runAllReductionTests :: IO ()
runAllReductionTests = do
  putStrLn "=== Detailed Reduction Behavior Comparison ==="
  putStrLn ""

  testSimpleApplicationReduction
  testComplexReduction
  testNestedLetReduction
  testHigherOrderReduction

-- |
-- *** Core Problem Demonstration

-- Demonstrate the issue with bound variables being accidentally focused
demonstrateBindingIssue :: IO ()
demonstrateBindingIssue = do
  putStrLn "Demonstrating the Binding Variable Focusing Issue"
  putStrLn "================================================"
  putStrLn ""
  putStrLn "Consider the expression: \\x -> x + (1 + 2)"
  putStrLn "In this expression:"
  putStrLn "  - x is a bound variable, should remain unchanged"
  putStrLn "  - (1 + 2) is an expression that needs evaluation"
  putStrLn "  - x + ... also needs focusing, because the right side is not a value"
  putStrLn ""
  putStrLn "Unprotected implementation (focusing False):"
  putStrLn "  focusing False (Pmu bind) = Pmu \\a -> focusing False (bind a)"
  putStrLn "  This means bound variables might be accidentally further focused"
  putStrLn ""
  putStrLn "Protected implementation with Pstx/Cstx (focusing True):"
  putStrLn "  focusing True (Pmu bind) = Pmu \\a -> focusing True (bind (Cstx a))"
  putStrLn "  Cstx marks that a is a syntax variable, should not be further focused"
  putStrLn ""
  testVariableInRedex
  putStrLn "To really see the difference, we need to compare the reduction behaviors!"

-- |
-- *** Theoretical Problem Demonstration

demonstrateTheoreticProblem :: IO ()
demonstrateTheoreticProblem = do
  putStrLn "Theoretical Problem Demonstration"
  putStrLn "================================"
  putStrLn ""
  putStrLn "Although it's hard to directly observe erroneous behavior in this"
  putStrLn "specific implementation, the theoretical problem exists:"
  putStrLn ""
  putStrLn "1. Unprotected implementation: focusing False (bind a) recursively focuses the entire result"
  putStrLn "2. Protected version: focusing True (bind (Cstx a)) protects bound variables"
  putStrLn ""
  putStrLn "This protection becomes important in more complex language extensions,"
  putStrLn "especially when more syntax constructs are added."
  putStrLn ""
  putStrLn "The key insight is that syntax variables (from HOAS) should be treated"
  putStrLn "differently from computed values during the focusing transformation."

-- |
-- *** Main Test Interface

-- Run all tests (both syntax and reduction)
runAllTests :: IO ()
runAllTests = do
  runAllSyntaxTests
  putStrLn ""
  runAllReductionTests
