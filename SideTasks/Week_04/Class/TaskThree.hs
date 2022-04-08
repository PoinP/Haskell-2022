-- Write a function that generates a list made up of the numbers in the interval [x, y].

-- Implementation detail:

-- Solve the task in one line of code.

main::IO()
main = do
    print $ getClosedInterval 1 9 == [1, 2, 3, 4, 5, 6, 7, 8, 9]
    print $ getClosedInterval 9 1 == [1, 2, 3, 4, 5, 6, 7, 8, 9]

getClosedInterval :: Int -> Int -> [Int]
getClosedInterval x y = [min x y .. max x y ]