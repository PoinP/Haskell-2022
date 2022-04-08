-- Define an iterative function for calculating the sum of the digits of a non-negative number.

main :: IO()
main = do
    -- print $ sumDigitsIter (-13) -- error "n was negative"
    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6
    print $ sumDigitsIter 165243526 == 34

sumDigitsIter :: Int -> Int
sumDigitsIter x = acc x 0
    where
        acc :: Int -> Int -> Int
        acc 0 sum = sum
        acc n sum
            | n < 0     = error "n was negative"
            | otherwise = acc (div n 10) (sum + mod n 10)