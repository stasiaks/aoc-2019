module Main where
import Data.List.Split
import Data.Tuple.Strict
import Data.Char

data Direction = ToLeft | ToRight | Up | Down
  deriving Show
data Movement = Movement Direction Int
  deriving Show

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

tuplify :: [a] -> (a,a)
tuplify [x,y] = (x,y)

