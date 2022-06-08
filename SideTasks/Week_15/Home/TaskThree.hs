main :: IO()
main = do
    print $ (averageFunction [(+1), (**0.5), (2**)]) 2 == 2.804738

averageFunction :: (Fractional a, RealFrac a) => [(a -> a)] -> a -> a
averageFunction fs = (\n -> roundN (average $ map (\f -> f n) fs) 6)

average :: (Fractional a) => [a] -> a
average xs = (sum xs) / (fromIntegral (length xs))

roundN :: (Fractional a, RealFrac a) => a -> Int -> a
roundN x n = fromIntegral (round (x * (10 ^ n))) / (10 ^ n)