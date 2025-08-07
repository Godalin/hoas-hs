module Main (main) where

import           ComprehensiveFunTest

main :: IO ()
main = do
  putStrLn "Running Core and Fun Language Tests with Focusing Analysis"
  putStrLn "========================================================="
  putStrLn ""

  -- Run all tests
  runAllLanguageTests
