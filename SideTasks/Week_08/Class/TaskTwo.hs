main :: IO()
main = do
    print $ removeNb 26 == [(15,21),(21,15)]
    print $ removeNb 100 == []
    print $ removeNb 101 == [(55,91),(91,55)]

removeNb :: Int -> [(Int, Int)]
removeNb n = [(a, b) | a <- [1 .. n], b <- [1 .. n], a * b == sumN a b]
    where
        sumN a b = sum [1 .. n] - a - b