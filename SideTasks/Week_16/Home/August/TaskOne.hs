main :: IO()
main = do
    print $ seriesSum 1 3 == 20.0
    print $ seriesSum 2 4 == 64.0

seriesSum :: Double -> Int -> Double
seriesSum x n = sum [1 + x ^ i + fromIntegral i ^ 2 | i <- [1 .. n]]