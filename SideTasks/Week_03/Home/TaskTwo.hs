-- Define a function that sorts a number in descending order.

import Data.List

main :: IO()
main = do
    print $ sortN 1714 == 7411
    print $ sortN 123450 == 543210
    print $ sortN 123405 == 543210
    print $ sortN 123045 == 543210
    print $ sortN 120345 == 543210
    print $ sortN 102345 == 543210
    print $ sortN 8910 == 9810
    print $ sortN 321 == 321
    print $ sortN 29210 == 92210
    print $ sortN 1230 == 3210
    print $ sortN 55345 == 55543
    print $ sortN 14752 == 75421
    print $ sortN 329450 == 954320
    print $ sortN 9125 == 9521

sortN :: Int -> Int
sortN x = acc x 0 (digs x)
    where
        acc x n d
            | x == 0 && digs n /= d = acc x (n * 10) d
            | x == 0                = n
            | otherwise             = acc (removeFirstOccurrence x $ findMax x) (findMax x + n * 10) d

findMax :: Int -> Int
findMax x = acc x 0
    where
        acc 0 max = max
        acc x max
            | mod x 10 > max = acc (div x 10) (mod x 10)
            | otherwise      = acc (div x 10) max

removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence x y = acc x 0
    where
        acc 0 n = n
        acc x n
            | mod x 10 == y = acc 0 (n * (10 ^ digs (div x 10)) + div x 10)
            | otherwise     = acc (div x 10) (n * 10 + mod x 10)

digs :: Int -> Int
digs 0 = 0
digs x = 1 + digs (div x 10)

-- Old solution

-- reverseList :: [Int] -> [Int]
-- reverseList [] = []
-- reverseList (x:xs) = reverseList xs ++ [x]

-- numToList :: Int -> [Int]
-- numToList 0 = []
-- numToList x = mod x 10 : numToList(div x 10)

-- listToNum :: [Int] -> Int
-- listToNum [] = 0
-- listToNum xs = acc xs 0
--     where
--         acc :: [Int] -> Int -> Int
--         acc [] num     = num
--         acc (x:xs) num = acc xs (num * 10 + x)

-- sortN :: Int -> Int
-- sortN = listToNum . reverseList . sort . numToList