-- Given a divisor d and a bound b, find the largest integer N, such that:

-- *N* is divisible by *d*
-- and *N* is less than or equal to *b*
-- and *N* is greater than 0.

main :: IO()
main = do
    print $ maxMultiple 2 7 == 6
    print $ maxMultiple 3 10 == 9
    print $ maxMultiple 7 17 == 14
    print $ maxMultiple 10 50 == 50
    print $ maxMultiple 37 200 == 185
    print $ maxMultiple 7 100 == 98  
    print $ maxMultiple 7 10 == 7
    print $ maxMultiple 4 4 == 4

maxMultiple :: Int -> Int -> Int 
maxMultiple d b = acc b
    where
        acc :: Int -> Int
        acc n
            | mod n d == 0 = n
            | otherwise    = acc (n - 1)

maxMultiple' :: Int -> Int -> Int -- How does this even work????
maxMultiple' d = acc
    where
        acc :: Int -> Int
        acc n
            | mod n d == 0 = n
            | otherwise    = acc (n - 1)  