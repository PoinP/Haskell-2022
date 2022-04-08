-- Define a predicate that checks whether a non-negative number is a palindrome.
-- Hint:
-- A number is a palindrome if and only if it is the same number from right to left as well as from left to right.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant ==" #-}

main :: IO()
main = do
    print $ isPalindrome 1 == True
    print $ isPalindrome 6 == True
    print $ isPalindrome 1010 == False
    print $ isPalindrome 505 == True
    print $ isPalindrome 123321 == True
    print $ isPalindrome 654 == False
    print $ isPalindrome 121 == True
    print $ isPalindrome 12 == False
    print $ isPalindrome 120 == False
    print $ isPalindrome 12321 == True
    print $ isPalindrome 1221 == True

rev :: Int -> Int
rev x = acc x 0 where
    acc 0 rev = rev
    acc n rev = acc (div n 10) (rev * 10 + mod n 10)

isPalindrome :: Int -> Bool
isPalindrome x = rev x == x