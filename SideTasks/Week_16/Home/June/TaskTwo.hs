main :: IO()
main = do
    print $ interserctionPoints (\x -> x) (\x -> x * x) (-5) 5 == [0, 1]
    print $ interserctionPoints (\x -> x) (\x -> x * x + 1) (-5) 5 == []

interserctionPoints :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> [Int]
interserctionPoints f g min max = [x | (x, y) <- zip [f x | x <- [min .. max]] [g y | y <- [min .. max]], x == y]