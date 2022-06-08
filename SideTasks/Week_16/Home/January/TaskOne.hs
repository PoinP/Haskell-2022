main :: IO()
main = do
    print $ findNb 1071225 == 45
    print $ findNb 40539911473216 == 3568
    print $ findNb 135440716410000 == 4824
    print $ findNb 4183059834009 == 2022
    print $ findNb 91716553919377 == -1
    print $ findNb 24723578342962 == -1

findNb :: Integer -> Integer
findNb m = acc m 1
    where
        acc 0 c = c - 1
        acc m c
            | m < 0 = -1
            | otherwise = acc (m - c ^ 3) (c + 1)