module MyLib (lmmExample) where

import           LMM

lmmExample :: IO ()
lmmExample = reduceFunList defined (fcall "akm" [2, 2] [])
