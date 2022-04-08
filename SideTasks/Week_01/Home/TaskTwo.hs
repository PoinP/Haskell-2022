-- Define two functions that return the sum of the cubes of two whole numbers:

--      using a power operator;
--      without using a power operator.

main :: IO()
main = do
    print $ sumCubesPow 5 1 == 126
    print $ sumCubesPow 10 50 == 126000

    print $ sumCubesNoPow 5 1 == 126
    print $ sumCubesNoPow 10 50 == 126000

sumCubesPow :: Int -> Int -> Int
sumCubesPow x y = x ^ 3 + y ^ 3

sumCubesNoPow :: Int -> Int -> Int
sumCubesNoPow x y = (x * x * x) + (y * y * y)

-- sumCubesPow :: Int -> Int -> Int
-- sumCubesPow x y = (x ^ x) + (y ^ y)

-- sumCubesNoPow :: Int -> Int -> Int
-- sumCubesNoPow x y = acc x y 1 where
--     acc 0 0 result = result
--     acc 0 b result = acc 0 (b - 1) (result * y)
--     acc a 0 result = acc (a - 1) 0 (result * x)
--     acc a b result = acc (a - 1) (b - 1) (result * x * y)
    