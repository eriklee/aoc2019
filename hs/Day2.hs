module Day2 where

import qualified Data.List.Split as S
import qualified Data.IntMap.Strict as M

main :: IO ()
main = do
  putStr "Part 1: "
  print =<< day2_1 "input/day2.txt"
  putStr "Part 2: "
  print =<< day2_2 "input/day2.txt"

type IntMachine = M.IntMap Int

buildIM :: [Int] -> IntMachine
buildIM = M.fromList . zip [0..]

readInput :: String -> IO IntMachine
readInput = fmap (buildIM . fmap read . S.splitOn ",") . readFile

day2_1 :: String -> IO Int
day2_1 filename = do
  im <- readInput filename
  let im' = initMachine 12 2 im
  let im'' = runMachine 0 im'
  return $ im'' M.! 0

day2_2 :: String -> IO (Int, Int)
day2_2 filename = do
  im <- readInput filename
  let target = 19690720
  let inputs = [(a,b) | a <- [0..99]
                      , b <- [0..99]
                      , (target ==) . M.findWithDefault 0 0
                      . runMachine 0 . initMachine a b $ im]
  return $ head inputs

initMachine :: Int -> Int -> IntMachine -> IntMachine
initMachine a b im = M.adjust (const b) 2 $ M.adjust (const a) 1 im

runMachine :: Int -> IntMachine -> IntMachine
runMachine pc im | im M.! pc == 99 = im
runMachine pc im | im M.! pc == 1  = runMachine (pc + 4) $ add pc im
runMachine pc im | im M.! pc == 2  = runMachine (pc + 4) $ mult pc im

add :: Int -> IntMachine -> IntMachine
add = math (+)

math :: (Int -> Int -> Int) -> Int -> IntMachine -> IntMachine
math op pc im =
  M.adjust 
    (const $ (la im $ pc + 1) `op` (la im $ pc + 2))
    (im M.! (pc + 3)) im

la :: IntMachine -> Int -> Int
la mem ad = mem M.! (mem M.! ad)

mult :: Int -> IntMachine -> IntMachine
mult = math (*)
