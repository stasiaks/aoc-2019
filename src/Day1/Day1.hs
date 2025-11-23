module Main where

main :: IO ()
main = do
  contents <- getContents
  putStr $ show $ sum $ map (calculateFuel . read) (lines contents)

calculateFuel a = floor (a / 3) - 2