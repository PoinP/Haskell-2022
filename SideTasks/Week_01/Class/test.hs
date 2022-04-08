main :: IO()
main = do
    print $ fact 5

fact :: Int -> Int
fact x
    | x == 0 = 1
    | otherwise = x * fact(x - 1)