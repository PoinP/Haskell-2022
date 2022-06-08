main :: IO()
main = do
    print $ (poly [0,1,2,3]) 1 == 6
    print $ (poly [1,1,1,1,1]) 2 == 31
    print $ (poly [1,0,1,0]) 3 == 10
    print $ (poly [1,2,0,2,1]) 2 == 37

poly :: [Int] -> (Int -> Int)
poly xs = (\x -> sum [n * (x ^ i) | (n, i) <- zip xs [0 .. length xs]])