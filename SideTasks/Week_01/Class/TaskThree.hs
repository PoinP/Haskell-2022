-- Define a recursive and an iterative function for calculating the factorial of a non-negative number.

-- solve with a linearly recursive process;
-- solve using a linearly iterative process.

main :: IO()
main = do
    print $ factRec 11 == 39916800
    --print $ factRec (-11) -- error: x was negative
    print $ factIter 11 == 39916800
    --print $ factIter (-11) -- error: x was negative

factRec :: Int -> Int
factRec n 
    | n < 0 = error "x was negative"
    | n > 0 = n * factRec (n - 1)
    | otherwise = 1

factIter :: Int -> Int
factIter 0 = 1
factIter n
    | n < 0 = error "x was negative"
    | otherwise = helper n 1 where
        helper :: Int -> Int -> Int
        helper 0 result = result
        helper x result = helper (x - 1) (x * result)
