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

  -- Run all tests (both syntax comparison and reduction tests)
  runAllTests

  putStrLn ""
  putStrLn "The differences clearly show how Pstx/Cstx protection affects"
  putStrLn "the intermediate reduction steps while preserving final results."
