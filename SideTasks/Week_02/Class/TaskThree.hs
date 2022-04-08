-- Define a recursive and an iterative function for calculating x to the power of n, where x is real and n is positive.
-- need a bit more work

main :: IO()
main = do
    print $ powRec 2 5 == 32
    print $ powRec 15 3 == 3375

    print $ powIter 2 5 == 32
    print $ powIter 15 3 == 3375

    print $ powRec 2 0 == 1 -- should return an error (according to the task description)

powIter :: Int -> Int -> Int 
powIter x y = acc x y x where
    acc x 0 _ = 1
    acc x 1 sum = sum
    acc x y sum = acc x (y - 1) (sum * x)

powRec :: Int -> Int -> Int 
powRec x 1 = x
powRec x y 
    | y < 1 = error "The power must be a positive number!"
    | otherwise = x * powRec x (y - 1)