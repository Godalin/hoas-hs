module Main where

import qualified MyLib

main :: IO ()
main = do
  putStrLn "Hello, HOAS Calculi Zoo!"
  putStrLn "akm(2,2)"
  MyLib.lmmExample
