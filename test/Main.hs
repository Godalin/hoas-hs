module Main (main) where

import           Test_LMM

main :: IO ()
main = do
  putStrLn "Running Core and Fun Language Tests with Focusing Analysis"
  putStrLn "========================================================="
  putStrLn ""

  -- Run all tests
  runAllLanguageTests
