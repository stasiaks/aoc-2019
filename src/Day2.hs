module Main where
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
  contents <- getContents
  let input = map read $ splitOn "," contents
  putStrLn "Part 1:"
  putStrLn $ show $ fromJust $ part1 input -- I don't care much about safety here
  putStrLn "Part 2:"
  putStrLn $ show $ part2 input 

part1 :: [Int] -> Maybe Int
part1 xs = intcodeProgram xs 12 2

part2 :: [Int] -> Int
part2 xs = head [ (100 * a) + b | a <- [0..99], b <- [0..99], intcodeProgram xs a b == Just 19690720]

replaceInputs :: [Int] -> Int -> Int -> [Int]
replaceInputs xs a b = fill (fill xs 1 a) 2 b

intcodeProgram :: [Int] -> Int -> Int -> Maybe Int
intcodeProgram xs a b = exec (replaceInputs xs a b) 0 >>= \ys -> return (head ys)

value :: [Int] -> Int -> Int
value xs n = head $ drop n xs

referenceValue :: [Int] -> Int -> Int
referenceValue xs n = value xs $ value xs n

fill :: [Int] -> Int -> Int -> [Int]
fill xs a b = (take a xs) ++ [b] ++ (drop (a+1) xs)

operation :: [Int] -> Int -> (Int -> Int -> Int) -> [Int]
operation xs a f = fill xs (value xs (a+2)) $ f (referenceValue xs a) (referenceValue xs (a+1))

exec :: [Int] -> Int -> Maybe [Int]
exec xs n
  | (value xs n) == 1  = exec (operation xs (n+1) (+)) (n+4) -- Addition
  | (value xs n) == 2  = exec (operation xs (n+1) (*)) (n+4) -- Mutliplication
  | (value xs n) == 99 = Just xs                             -- Halt
  | otherwise          = Nothing