-- Define a function that returns the sum of the 
-- first n prime numbers that contain a digit d.

-- Implementation detail:

-- Solve using higher order functions in ONE line of code.

main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392 -- n = 5, d = 2
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

isPrime :: Int -> Bool
isPrime n = n > 1 && and [mod n x /= 0 | x <- [2 .. n - 1]]

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes x d = sum $ take x $ filter (\x -> isPrime x && elem (show d !! 0) (show x)) [1 .. 1000]