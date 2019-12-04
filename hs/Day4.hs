module Day4 where

main :: IO ()
main = do
  let cands = day4_1 353096 843212
  putStr "Part1: "
  print $ length $ cands
  putStr "Part1: "
  print $ length $ day4_2 cands

day4_1 :: Int -> Int -> [Int]
day4_1 s e = filter isGood [s..e]

day4_2 :: [Int] -> [Int]
day4_2 = filter isGood2

isGood :: Int -> Bool
isGood n = let s = show n in
  allIncreasing s && hasRepeat s

isGood2 :: Int -> Bool
isGood2 = hasTwoRepeat . show

allIncreasing :: String -> Bool
allIncreasing [] = True
allIncreasing [_] = True
allIncreasing (x:y:zs) = x <= y && allIncreasing (y:zs)

hasRepeat :: String -> Bool
hasRepeat = not . null . filter (>1) . rle

hasTwoRepeat :: String -> Bool
hasTwoRepeat = not . null . filter (==2) . rle

rle :: String -> [Int]
rle [] = []
rle (x:xs) = rle' 1 x xs

rle' :: Eq a => Int -> a -> [a] -> [Int]
rle' n _ [] = [n]
rle' n x (y:ys) | x == y = rle' (n+1) x ys
                | otherwise = n : rle' 1 y ys
