-- Define a function (sum-numbers start finish) that returns the 
-- sum of all numbers in the interval [start, finish] whose digits
-- are ordered in descending order via a linearly recursive process.

main :: IO()
main = do
    print $ sumNumbers 1 9 == 45
    print $ sumNumbers 199 203 == 200
    print $ sumNumbers 219 225 == 663
    print $ sumNumbers 225 219 == 663

descDigits :: Int -> Bool 
descDigits x = acc x
    where
        acc :: Int -> Bool
        acc x
            | x < 10                        = True
            | mod x 10 > mod (div x 10) 10  = False 
            | otherwise                     = acc (div x 10)

sumNumbers :: Int -> Int -> Int 
sumNumbers x y = acc (min x y)
    where
        acc :: Int -> Int
        acc n
            | n > max x y  = 0
            | descDigits n = n + acc (n + 1)
            | otherwise    = acc (n + 1) 