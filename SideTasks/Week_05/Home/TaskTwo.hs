-- Check whether a seqence of numbers forms an arithmetic progression.

-- IF-ELSE AND GUARDS THAT RETURN TRUE OR FALSE ARE FORBIDDEN


main :: IO()
main = do
    print $ isArithmetic [3] == True
    print $ isArithmetic [3, 5] == True
    print $ isArithmetic [1, 2, 3, 4, 5] == True
    print $ isArithmetic [3, 5, 7, 9, 11] == True
    print $ isArithmetic [3, 5, 8, 9, 11] == False

isArithmetic :: [Int] -> Bool
isArithmetic []     = True
isArithmetic [x]    = True
isArithmetic xs     = xs == take (length xs) [xs !! 0, xs !! 1 ..]
