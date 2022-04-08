-- Write a function that removes every element equal to x in a list.

-- Implementation details:

-- - Solve using recursion.
-- - Solve using higher order functions.

main :: IO()
main = do
    print $ removeAllRec 5 [5] == []
    print $ removeAllRec 4 [4, 4] == []
    print $ removeAllRec 5 [1] == [1]
    print $ removeAllRec 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAllRec 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]

    print $ removeAllHOF 5 [5] == []
    print $ removeAllHOF 4 [4, 4] == []
    print $ removeAllHOF 5 [1] == [1]
    print $ removeAllHOF 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAllHOF 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]

    print $ removeAll 5 [5] == []
    print $ removeAll 4 [4, 4] == []
    print $ removeAll 5 [1] == [1]
    print $ removeAll 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAll 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]


removeAllRec :: Int -> [Int] -> [Int]
removeAllRec _ [] = []
removeAllRec n (x:xs)
    | n == x    = removeAllRec n xs
    | otherwise = x : removeAllRec n xs

removeAllHOF :: Int -> [Int] -> [Int]
removeAllHOF n = filter (/= n)

removeAll :: Int -> [Int] -> [Int]
removeAll n xs = [x | x <- xs, x /= n]