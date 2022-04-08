-- Define a function that removes the first occurrence
-- of a digit in a number by going from right to left.

main :: IO()
main = do
    print $ removeFirstOccurrence 15365 5 == 1536
    print $ removeFirstOccurrence 15360 0 == 1536
    print $ removeFirstOccurrence 15300 0 == 1530
    print $ removeFirstOccurrence 15365 1 == 5365
    print $ removeFirstOccurrence 35365 3 == 3565
    print $ removeFirstOccurrence 1212 1 == 122
    print $ removeFirstOccurrence 1212 2 == 121
    print $ removeFirstOccurrence (removeFirstOccurrence 1212 1) 1 == 22

reverse' :: Int -> Int
reverse' 0 = 0
reverse' x = acc x 0
    where
        acc :: Int -> Int -> Int
        acc 0 rev = rev
        acc x rev = acc (div x 10) (rev * 10 + mod x 10)

reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

numToList :: Int -> [Int]
numToList 0 = []
numToList x = mod x 10 : numToList(div x 10)

listToNum :: [Int] -> Int
listToNum [] = 0
listToNum xs = acc xs 0
    where
        acc :: [Int] -> Int -> Int
        acc [] num     = num
        acc (x:xs) num = acc xs (num * 10 + x)

removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence x y
    | x == y = 0
    | otherwise = acc (numToList x) y []
        where
            acc :: [Int] -> Int -> [Int] -> Int
            acc [] _ ns = listToNum (reverseList ns)
            acc (x:xs) y ns
                | x == y    = acc xs (-1) ns
                | otherwise = acc xs y (ns ++ [x])

-- This doesn't alawys work unfortunately so yeee
-- removeFirstOccurrence :: Int -> Int -> Int
-- removeFirstOccurrence x y = acc x y 0
--     where
--         acc :: Int -> Int -> Int -> Int
--         acc 0 _ res = reverse' res
--         acc x y res
--             | mod x 10 == y = acc (div x 10) (-1) res
--             | otherwise     = acc (div x 10) y (res * 10 + mod x 10)