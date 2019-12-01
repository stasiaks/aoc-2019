module Main where

main :: IO ()
main = do
  contents <- getContents
  let input = map read $ lines contents
  putStrLn "Part 1:"
  putStrLn $ show $ part1 $ input
  putStrLn "Part 2:"
  putStrLn $ show $ part2 $ input

part1 xs = sum $ map calculateFuel xs

--part2 xs = cfTR (calculateFuel $ part1 $ xs) (part1 $ xs)
part2 xs = sum $ map (accountForFuel . calculateFuel) xs

calculateFuel :: Integer -> Integer
calculateFuel a = (div a 3)- 2

accountForFuel :: Integer -> Integer
accountForFuel a = cfTR (calculateFuel a) a

cfTR :: Integer -> Integer -> Integer
cfTR a b 
    | a >= 0 = cfTR (calculateFuel a) (a + b)
    | otherwise = b