main :: IO()
main = do
    print $ f 3 == 13
    print $ f 1 == 11
    print $ f 8 == 0


f :: Int -> Int
f = listToFunction [1, 2, 3]

listToFunction :: [Int] -> (Int -> Int)
listToFunction lst = (\x -> sum [xi + 10 | xi <- lst, xi == x] )