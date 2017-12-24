import Data.Maybe
import Data.Sequence
import qualified Data.Map as Map

redistribute :: Seq Int -> Seq Int
redistribute seqq = redistributeAux startSeq maxIdx value toTake
  where maxIdx = fromMaybe 0 (elemIndexL (maximum seqq) seqq)
        value = maximum seqq
        toTake = max 1 (value `div` ((Data.Sequence.length seqq)-1))
        startSeq = update maxIdx 0 seqq

redistributeAux :: Seq Int -> Int -> Int -> Int -> Seq Int
redistributeAux seqq _ 0 _ = seqq
redistributeAux seqq idx left toTake =
  redistributeAux newSeq newIndex newLeft newToTake
  where newIndex = getNewIndex seqq idx
        newToTake = min toTake left
        newLeft = left - newToTake
        newSeq = adjust (+ newToTake) newIndex seqq

getNewIndex :: Seq Int -> Int -> Int
getNewIndex seqq idx = mod (idx + 1) (Data.Sequence.length seqq)

store :: Seq Int -> Int -> Map.Map (Seq Int) Int ->(Map.Map (Seq Int) Int, Bool)
-- | Inserts the current memory state into the auxiliary set
-- Returns the new Set and a Bool if the state existed already
store valueList iters m
  | Map.member valueList m = (m, True)
  | otherwise = ((Map.insert valueList iters m), False)

solveAux :: Seq Int -> Map.Map (Seq Int) Int -> Int -> Int
solveAux memoryState auxMap iterations
  | seenBefore = iterations - fromJust (Map.lookup memoryState auxMap)
  | otherwise = solveAux newState newMap (iterations + 1)
  where newState = redistribute memoryState
        (newMap, seenBefore) = store memoryState iterations auxMap

solve :: Seq Int -> Int
solve memoryState =
  solveAux memoryState Map.empty 0

main :: IO()
main = do
  putStrLn (show (solve content))
  where content = fromList [4, 1, 15, 12, 0, 9, 9, 5, 5, 8, 7, 3, 14, 5, 12, 3]
