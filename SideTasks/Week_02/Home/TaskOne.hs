-- Define a recursive and an iterative function for calculating the number of digits of a non-negative number.
-- Implementation detail:
-- Use guards!

main :: IO()
main = do
    --print $ countDigitsIter (-13) -- error "n was negative"
    print $ countDigitsIter 12345 == 5
    print $ countDigitsIter 123 == 3

    --print $ countDigitsRec (-13) -- error "n was negative"
    print $ countDigitsRec 12345 == 5
    print $ countDigitsRec 123 == 3

countDigitsIter :: Int -> Int
countDigitsIter x = acc x 0
    where
        acc :: Int -> Int -> Int
        acc 0 sum = sum
        acc n sum
            | n < 0     = error "n was negative"
            | otherwise = acc (div n 10) (sum + 1)

countDigitsRec :: Int -> Int
countDigitsRec x
    | x < 0     = error "n was negative"
    | x == 0    = 0
    | otherwise = 1 + countDigitsRec (div x 10)