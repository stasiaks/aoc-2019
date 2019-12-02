module Main where
import Data.List.Split

main :: IO ()
main = do
  contents <- getContents
  let input = map read (splitOn "," contents) :: [Integer]
  putStrLn $ show input