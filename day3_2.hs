import qualified Data.Map as Map

main :: IO()

up :: (Int, Int) -> (Int, Int)
up (a, b) = (a, b+1)
down :: (Int, Int) -> (Int, Int)
down (a, b) = (a, b-1)
left :: (Int, Int) -> (Int, Int)
left (a, b) = (a-1, b)
right :: (Int, Int) -> (Int, Int)
right (a, b) = (a+1, b)

listMovements :: Int -> [(Int, Int) -> (Int, Int)]
listMovements 0 = []
listMovements n = right :
  [up | _ <- [1 .. (1 + (2*(n-1)))]] ++
  [left | _ <- [1 .. (2 + (2*(n-1)))]] ++
  [down | _ <- [1 .. (2 + (2*(n-1)))]] ++
  [right | _ <- [1 .. (2 + (2*(n-1)))]]

listTotalMovements :: Int -> [(Int, Int) -> (Int, Int)]
listTotalMovements n = concat [listMovements x | x <- [1..n]]

applyMovements :: [(Int, Int) -> (Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
applyMovements [] l = l
applyMovements _ [] = []
applyMovements (x:xs) l = applyMovements xs ((x (head l)) : l)

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (a,b) = [(a+x,b+y) | x<-[-1..1], y<-[-1..1], (x,y) /= (a,b)]

value :: [(Int, Int)] -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
value [] m = m
value ((0, 0):xs) m = value xs (Map.insert (0, 0) 1 m)
value ((x, y):xs) m = value xs (Map.insert (x, y) newV m)
  where newV = sum [Map.findWithDefault 0 z m | z<-neighbours(x,y) ]

filteredToList :: Int -> Map.Map (Int, Int) Int -> [Int]
filteredToList n m = [y | (_,y) <- Map.toList m, y>=n]

moves :: [(Int, Int)]
moves = reverse (applyMovements (listTotalMovements 10) [(0,0)])


main = do putStrLn (show (minimum (filteredToList 347991 (value moves (Map.fromList [((0,0), 1)])))))

