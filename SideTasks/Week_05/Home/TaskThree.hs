-- Define a function that returns the sum of the special numbers in
-- the interval [x, y] (x <= y). A number is special if it contains 6
-- and can be expressed as 4k + 1, where k is a whole number.

-- Implementation detail: Solve the task with one line of code.

main :: IO()
main = do
    print $ specialSum 1 100 == 195 -- 61, 65, 69

hasSix :: Int -> Bool
hasSix n = elem '6' $ show n

canBeExpressed :: Int -> Bool
canBeExpressed n = mod (n - 1) 4 == 0

specialSum :: Int -> Int -> Int
specialSum x y = sum [n | n <- [x .. y], hasSix n, canBeExpressed n]