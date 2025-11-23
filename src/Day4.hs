module Main where
import Data.Digits
import Data.List.Split

main :: IO ()
main = do
  contents <- getContents
  let [inputFrom, inputTo] = map read $ splitOn "-" contents
  putStrLn "Part 1:"
  putStrLn $ show $ part1 inputFrom inputTo

part1 :: Int -> Int -> Int
part1 a b = 0