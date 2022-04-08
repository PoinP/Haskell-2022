-- Write a function that accepts three whole numbers - a, b and n and returns 
-- the sum of the last three numbers from the following sequence.
-- n will always be > 3.

main :: IO()
main = do
    print $ findSum 0 2 10 == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5 == 174 -- 26 + 50 + 98

calculateEq :: Int -> Int -> Int -> Int
calculateEq a _ 0 = a
calculateEq a b n = 2 ^ (n - 1) * b + calculateEq a b (n - 1)

findSum :: Int -> Int -> Int -> Int 
findSum a b n
    | n <= 4 = error "n must be bigger than 3 -> n > 3"
    | otherwise = acc n 0 0
        where
            acc _ 3 sum = sum
            acc n count sum = acc (n - 1) (count + 1) (sum + calculateEq a b n)