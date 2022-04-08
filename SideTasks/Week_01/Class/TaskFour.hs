-- Define a recursive and an iterative function for calculating the number at index i in 
-- the Fibonacci sequence (0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, ...).

-- Implementation detail: The input will always be valid, i.e. greater than -1.

main :: IO()
main = do
    print $ fibRec 11 == 89
    print $ fibIter 11 == 89
    print $ fibIter 110 == 43566776258854844738105

fibRec :: Integer -> Integer
fibRec 0 = 0
fibRec 1 = 1
fibRec n
    | n < 0 = error "Invalid input!"
    | otherwise = fibRec (n - 1) + fibRec (n - 2)

fibIter :: Integer -> Integer
fibIter n
    | n < 0 = error "Invalid input!"
    | otherwise = acc n 0 1 where
        acc :: Integer -> Integer -> Integer -> Integer
        acc 1 first second = second
        acc x first second = 
            acc (x - 1) second (first + second)