-- Write a predicate that checks whether an element is present in a list.

-- Implementation detail:

-- - Solve using a linearly recursive process WITHOUT pattern matching.
-- - Solve using a linearly recursive process WITH pattern matching.
-- - Solve using functions that work with lists.

main :: IO()
main = do
    print $ isPresentRecNonPM 0 [] == False
    print $ isPresentRecNonPM 0 [1, 2, 3] == False
    print $ isPresentRecNonPM 0 [0, -1, 2] == True

    print $ isPresentRecPM 0 [] == False
    print $ isPresentRecPM 0 [1, 2, 3] == False
    print $ isPresentRecPM 0 [0, -1, 2] == True

    print $ isPresentFunc 0 [] == False
    print $ isPresentFunc 0 [1, 2, 3] == False
    print $ isPresentFunc 0 [0, -1, 2] == True

isPresentRecNonPM :: Int -> [Int] -> Bool
isPresentRecNonPM n xs
    | null xs      = False
    | n == head xs = True
    | otherwise    = isPresentRecNonPM n $ tail xs

isPresentFunc :: Int -> [Int] -> Bool
isPresentFunc = elem

isPresentRecPM :: Int -> [Int] -> Bool
isPresentRecPM _ []     = False
isPresentRecPM n (x:xs) = x == n || isPresentRecPM n xs

-- Old function
isPresentRecPM' :: Int -> [Int] -> Bool
isPresentRecPM' _ []     = False
isPresentRecPM' n (x:xs)
    | n == x    = True
    | otherwise = isPresentRecPM n xs