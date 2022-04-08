-- Define a function that returns the sum of all prime divisors of a given number.

main :: IO()
main = do
    print $ sumPrimeDivs 0 == 0
    print $ sumPrimeDivs 6 == 5 -- 2 + 3
    print $ sumPrimeDivs 18 == 5 -- 2 + 3
    print $ sumPrimeDivs 19 == 19
    print $ sumPrimeDivs 45136 == 53

isPrime :: Int -> Bool 
isPrime x = x > 1 && acc (floor $ sqrt $ fromIntegral x)
    where
        acc :: Int -> Bool
        acc 1 = True
        acc n
            | mod x n == 0 = False 
            | otherwise    = acc (n - 1)


sumPrimeDivs :: Int -> Int 
sumPrimeDivs x = acc x 0
    where
        acc 0 sum = sum
        acc n sum
            | isPrime n && mod x n == 0 = acc (n - 1) (sum + n)
            | otherwise                 = acc (n - 1) sum