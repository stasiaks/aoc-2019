module Main where
import Data.Char
import Data.List.Split
import Data.Tuple.Strict

data Direction = ToLeft | ToRight | Up | Down
  deriving Show
data Movement = Movement Direction Int
  deriving Show
type Coordinate = (Int, Int)

instance Read Direction where
  readsPrec _ input = case splitAt 1 input of
    ("L", rest) -> [(ToLeft, rest)]
    ("R", rest) -> [(ToRight, rest)]
    ("U", rest) -> [(Up, rest)]
    ("D", rest) -> [(Down, rest)]
    otherwise -> []

instance Read Movement where
  readsPrec _ input = case mapSnd (span isDigit) $ splitAt 1 input of
    (dir, (digits, rest)) -> [(Movement (read dir) (read digits), rest)]

main :: IO ()
main = do
  contents <- getContents
  let input = tuplify $ map ((map (read :: String -> Movement)) . splitOn ",") (lines contents)
  putStrLn "Part 1:"
  putStrLn $ show input

part1 :: ([Movement], [Movement]) -> Int
part1 (xs, ys) = 1

coordinatesTouched :: [Movement] -> [Coordinate] -> Coordinate -> [Coordinate]
coordinatesTouched (m:ms) cs c = coordinatesTouched ms (newCoordinates ++ cs) (head newCoordinates)
  where newCoordinates = movementToCoordinates m [] c
coordinatesTouched [] cs _ = cs

movementToCoordinates :: Movement -> [Coordinate] -> Coordinate -> [Coordinate]
movementToCoordinates (Movement _ 0) cs _ = cs
movementToCoordinates (Movement dir n) cs (x,y) = movementToCoordinates (Movement dir (n-1)) (newCoordinate : cs) newCoordinate
  where newCoordinate = case dir of
          ToLeft  -> (x-1, y)
          ToRight -> (x+1, y)
          Up      -> (x, y+1)
          Down    -> (x, y-1)

tuplify :: [a] -> (a,a)
tuplify [x,y] = (x,y)

