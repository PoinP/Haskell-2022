-- Define a predicate that checks whether a number is perfect.

main :: IO()
main = do
    print $ isPerfect 1 == False -- the sum of the divisors is 0, because of the hint
    print $ isPerfect 6 == True -- 1 + 2 + 3 = 6 = 6
    print $ isPerfect 495 == False
    print $ isPerfect 33550336 == True

isPerfect :: Int -> Bool 
isPerfect x = acc (div x 2) 0
    where
        acc 0 sum = sum == x 
        acc n sum
            | mod x n == 0 = acc (n - 1) (sum + n)
            | otherwise = acc (n - 1) sum