import Data.Set (fromList)

main :: IO()

breakInput :: [Char] -> [[String]]
breakInput input = [words line | line <- lines input]

hasDupes :: [String] -> Bool
hasDupes line = length line /= length (fromList line)

countValues :: [[String]] -> Int -> Int
countValues [] count = count
countValues (x:xs) count
  | not (hasDupes x) = countValues xs (count + 1)
  | otherwise = countValues xs count

main = do
  input <- readFile "day4_input"
  putStrLn (show (countValues (breakInput input) 0))
