-- A digital root is the recursive sum of all the digits in a number.
-- Given n, take the sum of the digits of n. If that value has more 
-- than one digit, continue reducing in this way until a single-digit 
-- number is produced. This is only applicable to the natural numbers.

main :: IO()
main = do
    print $ digitalRoot 16 == 7
    -- => 1 + 6
    -- => 7
    print $ digitalRoot 942 == 6
    -- => 9 + 4 + 2
    -- => 15 ...
    -- => 1 + 5
    -- => 6
    print $ digitalRoot 132189 == 6
    print $ digitalRoot 493193 == 2

recursiveSum :: Int -> Int 
recursiveSum x
    | x < 0     = error "x must be a positive integer"
    | x < 10    = x
    | otherwise = mod x 10 + recursiveSum (div x 10)

digitalRoot :: Int -> Int 
digitalRoot x
    | x < 0  = error "x must be a positive integer"
    | x < 10 = x
    | otherwise = digitalRoot (recursiveSum x)