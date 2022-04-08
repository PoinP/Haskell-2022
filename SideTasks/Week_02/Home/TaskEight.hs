-- Write a function that removes the digit d from the number n.

main :: IO()
main = do
    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0
    print $ removeD 2 1234 == 134
    print $ removeD 4 14452424 == 1522

reverse' :: Int -> Int 
reverse' x = acc x 0
    where
        acc :: Int -> Int -> Int
        acc 0 rev = rev
        acc x rev = acc (div x 10) (rev * 10 + mod x 10)

removeD :: Int -> Int -> Int
removeD num x = reverse' (acc x 0)
    where
        acc :: Int -> Int -> Int
        acc 0 rem = rem
        acc x rem
            | mod x 10 == num = acc (div x 10) rem
            | otherwise       = acc (div x 10) (rem * 10 + mod x 10)