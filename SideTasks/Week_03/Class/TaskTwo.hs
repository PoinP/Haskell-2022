-- Define a function that accepts two whole
-- numbers x and n and calculates the following 
-- sum via a linearly iterative process:

-- 1 + x + x^2 + x^3 + ... + x^n

main :: IO()
main = do
    print $ calculateSum 5 0 == 1 -- x=5, n=0
    print $ calculateSum 5 1 == 6
    print $ calculateSum 10 1 == 11
    print $ calculateSum 1 11 == 12
    print $ calculateSum 2 11 == 4095

calculateSum :: Int -> Int -> Int 
calculateSum x n = acc n 1
    where
        acc :: Int -> Int -> Int
        acc 0 sum   = sum
        acc pow sum = acc (pow - 1) (sum + x ^ pow)