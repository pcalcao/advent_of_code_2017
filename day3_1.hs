main :: IO()

lowestInCircle :: Int -> Int
lowestInCircle 0 = 1
lowestInCircle 1 = 2
lowestInCircle n = lowestInCircle (n-1) + (8 * (n-1))

numbersInCircle :: Int -> [Int]
numbersInCircle n = [lowestInCircle (n-1) .. (lowestInCircle n) - 1]

findCircle :: Int -> Int
findCircle n = length (takeWhile (<= n) (map lowestInCircle [0..]))

getMiddleOfCircle :: Int -> [Int]
getMiddleOfCircle n = [l !! x | x <- [mult * x + (n - 2)|x<-[0..3]]]
  where l = numbersInCircle n
        mult = div (length l) 4

distanceToCenter :: Int -> [Int] -> Int
distanceToCenter n l = minimum [abs(x - n) | x <- l]

manhattanDistance :: Int -> Int
manhattanDistance x = circle + distanceToCenter x (getMiddleOfCircle circle) - 1
  where circle = findCircle x

main = do putStrLn (show (manhattanDistance 347991))
