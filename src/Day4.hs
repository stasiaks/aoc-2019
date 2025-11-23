module Main where
import Data.Digits
import Data.List
import Data.List.Split

main :: IO ()
main = do
  contents <- getContents
  let [inputFrom, inputTo] = map read $ splitOn "-" contents
  putStrLn "Part 1:"
  putStrLn $ show $ part1 inputFrom inputTo
  putStrLn "Part 2:"
  putStrLn $ show $ part2 inputFrom inputTo

part1 :: Int -> Int -> Int
part1 a b = length $ filter (\x -> neverDecreases x && twoAdjacentSame x) $ map (digits 10) [a..b]

part2 :: Int -> Int -> Int
part2 a b = length $ filter (\x -> neverDecreases x && onlyTwoAdjacentSame x) $ map (digits 10) [a..b]

neverDecreases :: Ord a => [a] -> Bool
neverDecreases xs = all (==True) $ zipWith (\x y -> x <= y) xs (tail xs)

twoAdjacentSame :: Eq a => [a] -> Bool
twoAdjacentSame xs = any (\x -> length x >= 2) $ group xs

onlyTwoAdjacentSame :: Eq a => [a] -> Bool
onlyTwoAdjacentSame xs = any (\x -> length x == 2) $ group xs