-- Write functions that return the number of elements in a list.

-- Implementation detail:

-- - Solve using a linearly recursive process WITHOUT pattern matching.
-- - Solve using a linearly recursive process WITH pattern matching.
-- - Solve using functions.
main :: IO()
main = do
    print $ myLengthRecNonPM [] == 0
    print $ myLengthRecNonPM [1, 2, 3] == 3

    print $ myLengthRecPM [] == 0
    print $ myLengthRecPM [1, 2, 3] == 3

    print $ myLengthFunc [] == 0
    print $ myLengthFunc [1, 2, 3] == 3

myLengthRecNonPM :: [Int] -> Int
myLengthRecNonPM xs
    | null xs   = 0
    | otherwise = 1 + myLengthRecNonPM (tail xs)

myLengthRecPM :: [Int] -> Int
myLengthRecPM [] = 0
myLengthRecPM (_:xs) = 1 + myLengthRecPM xs

myLengthFunc :: [Int] -> Int
myLengthFunc = length