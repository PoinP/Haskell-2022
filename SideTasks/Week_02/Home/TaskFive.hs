-- -- Define a predicate that checks whether two numbers are amicable.
-- Hint:
-- Two numbers are amicable if the sum of the divisors of one of them is equal to the other.

main :: IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True

sumDivisors :: Int -> Int
sumDivisors x = acc (div x 2) x
    where
        acc :: Int -> Int -> Int
        acc 0 sum = sum
        acc n sum
            | mod x n == 0 = acc (n - 1) (sum + n)
            | otherwise    = acc (n - 1) sum

areAmicable :: Int -> Int -> Bool 
areAmicable x y = sumDivisors x == sumDivisors y
