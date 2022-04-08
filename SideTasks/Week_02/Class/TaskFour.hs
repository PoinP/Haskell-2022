-- Define a predicate that checks whether a number is prime.
-- Hint:
-- A number - n, is prime if and only if it is natural and its only divisors are 1 and n.

main :: IO()
main = do
    print $ isPrime 1 == False
    print $ isPrime 2 == True
    print $ isPrime 3 == True
    print $ isPrime 6 == False
    print $ isPrime 61 == True

isPrime :: Int -> Bool 
isPrime x = x > 1 && helper 2 where
    helper :: Int -> Bool
    helper n
        | n > div x 2 = True
        | mod x n == 0 = False
        | otherwise = helper (n + 1)

isPrime' :: Int -> Bool 
isPrime' x = x > 1 && acc (floor $ sqrt $ fromIntegral x)
    where
        acc :: Int -> Bool
        acc 1 = True
        acc n
            | mod x n == 0 = False 
            | otherwise = acc (n - 1)
