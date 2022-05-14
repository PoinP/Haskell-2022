-- Define a predicate that accepts a number and returns if and only if the number is prime 
-- and every number that is produced by removing digits from right to left is prime.

main :: IO()
main = do
    print $ truncatablePrime 3797 == True -- 3797, 379, 37 and 3 are all prime
    print $ truncatablePrime 47 == False -- 47 is prime, but 4 is not

isPrime :: Int -> Bool 
isPrime x = x > 2 && acc (floor $ sqrt $ fromIntegral x)
    where
        acc :: Int -> Bool
        acc 1 = True
        acc n
            | mod x n == 0 = False
            | otherwise    = acc (n - 1)


-- Добре е да напишеш truncatablePrime без гардове.
truncatablePrime :: Int -> Bool
truncatablePrime 0 = True
truncatablePrime x = isPrime x && truncatablePrime' (div x 10)

truncatablePrime' :: Int -> Bool
truncatablePrime' 0 = True
truncatablePrime' x
    | isPrime x = truncatablePrime (div x 10)
    | otherwise = False