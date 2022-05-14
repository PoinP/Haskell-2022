-- Write a function that finds the sum of the elements in a list.

-- Implementation details:

-- - Solve using a linearly recursive process WITHOUT pattern matching.
-- - Solve using a linearly recursive process WITH pattern matching.
-- - Solve using functions that work with lists.

main :: IO()
main = do
    print $ mySumRecNonPM [] == 0
    print $ mySumRecNonPM [1, 2, 3] == 6

    print $ mySumRecPM [] == 0
    print $ mySumRecPM [1, 2, 3] == 6

    print $ mySumFunc [] == 0
    print $ mySumFunc [1, 2, 3] == 6

mySumRecNonPM :: [Int] -> Int
mySumRecNonPM xs
    | null xs   = 0
    | otherwise = head xs + mySumRecNonPM (tail xs)

mySumRecPM :: [Int] -> Int
mySumRecPM []     = 0
mySumRecPM (x:xs) = x + mySumRecPM xs

mySumFunc :: [Int] -> Int
mySumFunc = sum
