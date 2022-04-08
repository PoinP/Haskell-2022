-- Define a function that returns the sum of the divisors of a non-negative number.

main :: IO()
main = do
    print $ sumDivs 0 == 0
    print $ sumDivs 1 == 1
    print $ sumDivs 6 == 12 -- 1 + 2 + 3 + 6
    print $ sumDivs 12345 == 19776

sumDivs :: Int -> Int 
sumDivs x = acc (div x 2) x
    where
        acc 0 sum = sum
        acc n sum
            | mod x n /= 0 = acc (n - 1) sum
            | otherwise = acc (n - 1) (sum + n)