import Data.Sequence

hop :: Seq Int -> Int -> Int -> Int
hop seqq current steps
  | current >= Data.Sequence.length seqq = steps
  | current < 0 = steps
  | otherwise = hop newSequence (current + (index seqq current)) (steps + 1)
  where newSequence = update current ((index seqq current) + 1) seqq

main :: IO()
main = do
  content <- (readFile "day5_input")
  let input = fromList [read x | x <- (lines content)]
  putStrLn (show (hop input 0 0))
