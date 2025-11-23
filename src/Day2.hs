module Main where
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
  contents <- getContents
  let input = map read (splitOn "," contents) :: [Int]
  putStrLn "Part 1:"
  putStrLn $ show $ fromJust $ part1 input -- I don't care much about safety here

part1 :: [Int] -> Maybe Int
part1 xs = (exec (alarm1202 xs) 0) >>= \ys -> return (head ys)

alarm1202 :: [Int] -> [Int]
alarm1202 xs = fill (fill xs 1 12) 2 2

exec :: [Int] -> Int -> Maybe [Int]
exec xs n
  | (value xs n) == 1  = exec (addition       xs (n+1)) (n+4) -- Addition
  | (value xs n) == 2  = exec (multiplication xs (n+1)) (n+4) -- Mutliplication
  | (value xs n) == 99 = Just xs                              -- Halt
  | otherwise          = Nothing

referenceValue :: [Int] -> Int -> Int
referenceValue xs n = value xs $ value xs n

value :: [Int] -> Int -> Int
value xs n = head $ drop n xs

fill :: [Int] -> Int -> Int -> [Int]
fill xs a b = (take a xs) ++ [b] ++ (drop (a+1) xs)

operation xs a b c f = fill xs (value xs c) (f (referenceValue xs a) (referenceValue xs b))

addition       xs a = operation xs a (a+1) (a+2) (+)
multiplication xs a = operation xs a (a+1) (a+2) (*)