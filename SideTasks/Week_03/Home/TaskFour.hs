-- Define a function (sum-divisible-numbers start finish k) 
-- that returns the sum of all numbers from the interval 
-- [start, finish] whose digits sum up to a number that 
-- is evenly divisible by k.

-- Implementation detail: Create a linearly recursive process.

main :: IO()
main = do
    print $ sumDivisibleNumbers 0 10 5 == 5
    print $ sumDivisibleNumbers 0 100 5 == 990
    print $ sumDivisibleNumbers 100 0 5 == 990

sumDivisibleNumbers :: Int -> Int -> Int -> Int
sumDivisibleNumbers s f k = acc (min s f) 0
    where
        acc :: Int -> Int -> Int
        acc x sum
            | x > max s f              = sum
            | mod (sumDigits x) k == 0 = acc (x + 1) (sum + x)
            | otherwise                = acc (x + 1) sum 
            
sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits x = mod x 10 + sumDigits (div x 10)