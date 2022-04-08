-- Define a function that returns the sum of 
-- the smallest and greatest palindrome divisors 
-- of a natural number greater than 1.


------ !!!!!  This is most likely a shit solution :pp !!!!


main :: IO()
main = do
    print $ getPalindromes 132465 == 8
    print $ getPalindromes 654546 == 8
    print $ getPalindromes 100001 == 100012
    print $ getPalindromes 21612 == 21614
    print $ getPalindromes 26362 == 26364

isPalindrome :: Int -> Bool
isPalindrome x = show x == reverse (show x)

getFirstDivisor :: (Int -> Bool) -> Int -> Int
getFirstDivisor f x = acc 2
    where
        acc :: Int -> Int
        acc d
            | d == x              = x
            | mod x d == 0 && f d = d
            | otherwise           = acc (d + 1)

getLastDivisor :: (Int -> Bool) -> Int -> Int
getLastDivisor f x = acc x
    where
        acc :: Int -> Int
        acc 1 = x
        acc d
            | mod x d == 0 && f d = d
            | otherwise           = acc (d - 1)

getPalindromes :: Int -> Int
getPalindromes x = getFirstDivisor isPalindrome x + getLastDivisor isPalindrome x