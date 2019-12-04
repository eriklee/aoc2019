{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Day1 where
import Data.List (unfoldr)
import Control.Monad (join)

main :: IO ()
main = do
  putStr "Part 1: "
  print =<< day1_1 "input/day1.txt"
  putStr "Part 2: "
  print =<< day1_2 "input/day1.txt"

day1_1 :: String -> IO Int
day1_1 fn = sum . map (fuelRequired . read) . lines <$> readFile fn

fuelRequired :: Int -> Int
fuelRequired = (+ (-2)) . floor @Double . (/3) . fromIntegral

day1_2 :: String -> IO Int
day1_2 fn = sum . join . map (allFuel . read) . lines <$> readFile fn

isPos :: Int -> Maybe Int
isPos x = if x < 0 then Nothing else Just x

allFuel :: Int -> [Int]
allFuel = unfoldr (\x -> dupe <$> (isPos . fuelRequired) x)

dupe :: a -> (a,a)
dupe = join (,)
