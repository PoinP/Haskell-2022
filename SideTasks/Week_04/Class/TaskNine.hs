-- Define a function that returns the prime numbers 
-- in the range (x, y) that contain the digit 7.

-- - Solve using list comprehension in ONE line of code.
-- - Solve using higher order functions in ONE line of code.

main :: IO()
main = do
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]

isPrime :: Int -> Bool
isPrime x = x > 1 && and [mod x n /= 0 | n <- [2 .. x - 1]]

getPrimesLC :: Int -> Int -> [Int]
getPrimesLC x y = [n | n <- [min x y + 1 .. max x y - 1], isPrime n, elem '7' (show n)]

getPrimesHOF :: Int -> Int -> [Int]
getPrimesHOF x y = filter (\n -> isPrime n && elem '7' (show n)) [min x y + 1 .. max x y - 1]