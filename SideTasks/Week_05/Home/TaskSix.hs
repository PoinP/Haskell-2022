main :: IO()
main = do
    print $ switchSum (\x -> x + 1) (\x -> x * 2) 1 2
    print $ switchSum (\x -> x + 1) (\x -> x * 2) 2 2
    print $ switchSum (\x -> x + 1) (\x -> x * 2) 3 2
    print $ switchSum (\x -> x + 1) (\x -> x * 2) 4 2

switchSum :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
switchSum f g 0 = (\x -> x - x)
switchSum f g n = (\x -> generateSum f g n x + switchSum f g (n-1) x)

generateSum :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
generateSum f g 0 = id
generateSum f g n
    | odd n     = f . generateSum f g (n - 1)
    | otherwise = g . generateSum f g (n - 1)