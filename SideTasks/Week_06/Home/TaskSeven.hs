import Data.List
-- Write a function that sums the unique numbers in the sublists of a list.

main :: IO()
main = do
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 -- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45

timesSeen :: Int -> [Int] -> Int
timesSeen n [] = 0
timesSeen n (x:xs)
    | n == x    = 1 + timesSeen n xs
    | otherwise = timesSeen n xs

-- sumUnique :: [[Int]] -> Int
-- sumUnique xs = sum $ concat [[n | n <- ys, timesSeen n ys == 1 ] | ys <- xs]

sumUnique :: [[Int]] -> Int
sumUnique xs = sum $ concat $ filter (\xs -> length xs == 1) $ group $ sort $ concat xs
-- Not gonna lie, I am proud of this solution