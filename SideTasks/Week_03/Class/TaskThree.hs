-- Write a function that finds the maximum digit in a number.

main :: IO()
main = do
    print $ findMax 55345 == 5
    print $ findMax 14752 == 7
    print $ findMax 329450 == 9
    print $ findMax 9521 == 9

findMax :: Int -> Int
findMax x
    | x < 0 = error "x must be a positive integer!"
    | otherwise = acc x 0
        where
            acc :: Int -> Int -> Int
            acc 0 max = max
            acc x max
                | mod x 10 > max = acc (div x 10) (mod x 10)
                | otherwise      = acc (div x 10) max