import Data.Set (fromList)
import Data.List

main :: IO()

breakInput :: [Char] -> [[String]]
breakInput input = [words line | line <- lines input]

hasAnagrams :: [String] -> Bool
hasAnagrams line = hasDupes [sort s | s<-line]

hasDupes :: [String] -> Bool
hasDupes line = length line /= length (fromList line)

countValues :: [[String]] -> Int -> Int
countValues [] count = count
countValues (x:xs) count
  | not (hasAnagrams x) = countValues xs (count + 1)
  | otherwise = countValues xs count

main = do
  input <- readFile "day4_input"
  putStrLn (show (countValues (breakInput input) 0))
