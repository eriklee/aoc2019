module Day3 where

import Control.Monad (fail)
import Data.List
import Data.Either
import qualified Data.Set as S
import qualified Data.Map as M
import Text.Parsec

main :: IO ()
main = do
  putStr "Part 1: "
  print =<< day3_1 "input/day3.txt"
  putStr "Part 2: "
  print =<< day3_2 "input/day3.txt"

data Dir = DUp | DDown | DLeft | DRight deriving (Show)
data Run = Run Dir Int deriving (Show)
type Path = [Run]
type Coord = (Int, Int)

day3_1 :: String -> IO Int
day3_1 filename = do
  input <- readFile filename
  let paths = parsePaths $ lines input
  let pathSets = (getCoords . markPath) <$> paths
  let intersections = S.delete (0,0) $ foldl1' (S.intersection) pathSets
  return $ minimum $ manhattan <$> S.toList intersections

day3_2 :: String -> IO Int
day3_2 filename = do
  input <- readFile filename
  let paths = parsePaths $ lines input
  let pathSets = markPath <$> paths
  let intersections = S.delete (0,0) $ foldl1' (S.intersection) (getCoords <$> pathSets)
  let maps = buildMap <$> pathSets
  return $ minimum $ sumIntersections maps <$> S.toList intersections

sumIntersections :: [M.Map Coord Int] -> Coord -> Int
sumIntersections ms c = sum $ M.findWithDefault 1000 c <$> ms

buildMap :: S.Set (Coord, Int) -> M.Map Coord Int
buildMap = M.fromList . S.toList

getCoords :: S.Set (Coord, Int) -> S.Set Coord
getCoords = S.fromList . fmap fst . S.toList

markPath :: Path -> S.Set (Coord, Int)
markPath = markPath' (0,0) S.empty

markPath' :: Coord -> S.Set (Coord, Int) -> Path -> S.Set (Coord, Int)
markPath' p s rs = foldl' (flip S.insert) s $ coords p rs

coords :: (Int, Int) -> Path -> [(Coord, Int)]
coords p rs = zip (scanl move p $ dirs rs) [0..]

dirs :: [Run] -> [Dir]
dirs = concatMap (\(Run d n) -> replicate n d)

move :: Coord -> Dir -> Coord
move (x,y) DUp    = (x, y + 1)
move (x,y) DDown  = (x, y - 1)
move (x,y) DRight = (x + 1, y)
move (x,y) DLeft  = (x - 1, y)

manhattan :: Coord -> Int
manhattan (a,b) = abs a + abs b

-- Parser
parsePaths :: [String] -> [Path]
parsePaths = rights . map (parse path "")

path :: Parsec String () Path
path = parseRun `sepBy1` (oneOf ",")

parseDir :: Parsec String () Dir
parseDir = do
  c <- letter
  case c of
    'U' -> return DUp
    'D' -> return DDown
    'L' -> return DLeft
    'R' -> return DRight
    _ -> fail "Bad direction"

parseRun :: Parsec String () Run
parseRun = Run <$> parseDir <*> number

number :: Parsec String () Int
number = read <$> many1 digit
