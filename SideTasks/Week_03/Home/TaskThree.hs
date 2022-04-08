-- Define a function that accepts two real 
-- numbers and calculates the n-th partial sum from the following sequence:

main :: IO()
main = do
    print $ calcSeriesSum 1 0 == -2.0 -- x = 1, n = 0
    print $ calcSeriesSum 1 1 == -0.6666666666666667
    print $ calcSeriesSum 1 2 == -1.2000000000000002
    print $ calcSeriesSum 1 3 == -1.047619047619048
    print $ calcSeriesSum 1 4 == -1.0814814814814817
    print $ calcSeriesSum 1 5 == -1.0753246753246755
    print $ calcSeriesSum 1 6 == -1.0762718762718764

calcSeriesSum :: Int -> Int -> Double
calcSeriesSum x 0 = -2.0
calcSeriesSum x n = formula x n + calcSeriesSum x (n - 1)

formula :: Int -> Int -> Double
formula x n = fromIntegral (((-2) ^ (n + 1)) * x) / fromIntegral (calculateDenominator 3 n)

calculateDenominator :: Int -> Int -> Int
calculateDenominator x 0 = 1
calculateDenominator x n = x * calculateDenominator (x + 2) (n - 1)