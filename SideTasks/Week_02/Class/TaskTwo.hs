-- Define a recursive function for calculating the sum of the digits of a whole number.

main :: IO()
main = do
    print $ sumDigitsRec 12345 == 15
    print $ sumDigitsRec 123 == 6

sumDigitsRec :: Int -> Int
sumDigitsRec x = acc x 0 where
    acc 0 sum = sum
    acc x sum =
        acc (div x 10) (sum + mod x 10)