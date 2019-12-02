module Main where
import Data.List.Split
import Data.Array.ST

data Opcode = Addition
            | Multiplication
            | Halt
  deriving Show

main :: IO ()
main = do
  contents <- getContents
  let input = map read (splitOn "," contents) :: [Int]
  putStrLn $ show $ input
  putStrLn "Part 1:"
  putStrLn $ show $ part1 $ input

part1 xs = exec xs (opcode $ value xs 0) 0

exec :: [Int] -> Maybe Opcode -> Int -> Maybe [Int]
exec xs (Just Multiplication) n = exec (multiplication xs (n+1)) (opcode $ value xs (n+4)) (n+4)
exec xs (Just Addition)       n = exec (addition xs (n+1))       (opcode $ value xs (n+4)) (n+4)
exec xs (Just Halt)           _ = Just xs
exec _ Nothing                _ = Nothing

opcode :: Int -> Maybe Opcode
opcode 1  = Just Addition
opcode 2  = Just Multiplication
opcode 99 = Just Halt
opcode _  = Nothing

referenceValue :: [Int] -> Int -> Int
referenceValue xs n = value xs $ value xs n

value :: [Int] -> Int -> Int
value xs n = head $ drop n xs

fill :: [Int] -> Int -> Int -> [Int]
fill xs a b = (take a xs) ++ [b] ++ (drop (a+1) xs)

operation xs a b c f = fill xs (value xs c) (f (referenceValue xs a) (referenceValue xs b))

addition       xs a = operation xs a (a+1) (a+2) (+)
multiplication xs a = operation xs a (a+1) (a+2) (*)