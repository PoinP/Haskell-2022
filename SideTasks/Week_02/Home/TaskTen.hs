-- Define a function that takes two numbers and returns the number of palindromes between them.

main::IO()
main = do
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11

reverse' :: Int -> Int
reverse' x = acc x 0
    where
        acc :: Int -> Int -> Int
        acc 0 rev = rev
        acc x rev = acc (div x 10) (rev * 10 + mod x 10)

isPalindrome :: Int -> Bool
isPalindrome x = x == reverse' x

countPalindromes :: Int -> Int -> Int 
countPalindromes x y
    | x >= y    = acc (y + 1) x 0
    | otherwise = acc (x + 1) y 0
    where
        acc :: Int -> Int -> Int -> Int
        acc x y sum
            | x >= y         = sum
            | isPalindrome x = acc (x + 1) y (sum + 1)
            | otherwise      = acc (x + 1) y sum