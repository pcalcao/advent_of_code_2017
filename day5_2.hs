import Data.Sequence

hop :: Seq Int -> Int -> Int -> Int
hop seqq current steps
  | current >= Data.Sequence.length seqq = steps
  | current < 0 = steps
  | otherwise = hop newSequence (current + (index seqq current)) (steps + 1)
  where newSequence = updateSequence seqq current

updateSequence :: Seq Int -> Int -> Seq Int
updateSequence seqq n
  | value >= 3 = update n (value - 1) seqq
  | otherwise = update n (value + 1) seqq
  where value = index seqq n

main :: IO()
main = do
  content <- (readFile "day5_input")
  let input = fromList [read x | x <- (lines content)]
  let result = hop input 0 0
  putStrLn (show result)
