-- Define a function that accepts a natural number 
-- greater than 1 and returns a sorted list of prime 
-- factors the product of which gives the number.

import Data.List

main :: IO()
main = do
    print $ factorize 152 == [2, 2, 2,19]
    print $ factorize 123 == [3, 41]
    print $ factorize 13 == [13]

isPrime :: Int -> Bool
isPrime x = x > 1 && all (\xn -> mod x xn /= 0) [2 .. x-1]

primeFactors :: Int -> [Int]
primeFactors x = acc x 2
    where
        acc :: Int -> Int -> [Int]
        acc 1 _ = []
        acc x d
            | mod x d == 0 && isPrime d = d : acc (div x d) d
            | otherwise                 = acc x (d + 1)

factorize :: Int -> [Int]
factorize x
    | x <= 1    = error "Error: x must be greater than 1"
    | otherwise = primeFactors x