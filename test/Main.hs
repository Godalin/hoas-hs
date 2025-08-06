module Main (main) where

import           TestFocusing

main :: IO ()
main = do
  putStrLn "Running Focusing Tests..."
  putStrLn ""
  putStrLn "Note: Current implementation includes both focusing variants:"
  putStrLn "  - Protected focusing (True):  Uses Pstx/Cstx to protect bound variables"
  putStrLn "  - Unprotected focusing (False): Direct variable passing without protection"
  putStrLn ""

  -- Key test: Compare reduction behavior differences
  putStrLn "Key Test: Comparing Reduction Behavior Differences"
  putStrLn "=================================================="
  runAllReductionTests

  putStrLn ""
  putStrLn "The differences clearly show how Pstx/Cstx protection affects"
  putStrLn "the intermediate reduction steps while preserving final results."
