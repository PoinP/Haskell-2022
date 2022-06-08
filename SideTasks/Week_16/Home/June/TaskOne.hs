import Data.List

main :: IO()
main = do
    print $ biggestNumber [1,2,3,4,5] == 54321
    print $ biggestNumber [1,5,5,3,5] == 55531

biggestNumber :: [Int] -> Int
biggestNumber xs = listToInt $ foldr1 (\x1 x2 -> if listToInt x1 > listToInt x2 then x1 else x2) (permutations xs)

listToInt :: [Int] -> Int
listToInt xs = acc xs 0
    where
        acc [] num = num
        acc (x:xs) num = acc xs (num * 10 + x)