-- Define a function that sorts a number in descending order.

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

getBiggest :: Int -> Int 
getBiggest x = acc x 0
    where
        acc :: Int -> Int -> Int
        acc 0 max = max
        acc x max
            | mod x 10 > max = acc (div x 10) (mod x 10)
            | otherwise      = acc (div x 10) max

sortN :: Int -> Int
sortN x
    | x < 10 = x
    | otherwise = acc x 0
        where
            acc 0 n = 0 + (n * 10)
            acc x n = acc (removeFirstOccurrence x (getBiggest x)) (getBiggest x + (n * 10))