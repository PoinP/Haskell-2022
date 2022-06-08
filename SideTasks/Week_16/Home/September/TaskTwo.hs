main :: IO()
main = do
    print $ iterator [3, 4, 5] (+1) == True
    print $ iterator [1, 2, 4] (+1) == False

iterator :: (Num a, Eq a) => [a] -> (a -> a) -> Bool
iterator xs f = and [f x == y | (x, y) <- zip xs (tail xs)]