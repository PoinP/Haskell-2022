main :: IO()
main = do
    print $ removeEveryKth 3 [1 .. 9] == [1,2,4,5,7,8]
    print $ removeEveryKth 4 [1 .. 7] == [1,2,3,5,6,7]

removeEveryKth :: Int -> [a] -> [a]
removeEveryKth n xs = acc xs 1
    where
        acc [] _ = []
        acc (x:xs) i
            | i == n    = acc xs 1
            | otherwise = x : acc xs (i + 1)