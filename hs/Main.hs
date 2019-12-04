module Main where

import qualified Day1 as D1 (main)
import qualified Day2 as D2 (main)
import qualified Day3 as D3 (main)
import qualified Day4 as D4 (main)

main :: IO ()
main = do
  putStrLn "Day1: "
  D1.main
  putStrLn "Day2: "
  D2.main
  putStrLn "Day3: "
  D3.main
  putStrLn "Day4: "
  D4.main
