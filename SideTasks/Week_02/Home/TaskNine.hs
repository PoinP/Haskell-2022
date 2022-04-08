-- Write a function that forms a number from every other digit starting from the right of an integer n (n >= 10).

main :: IO()
main = do
    print $ everyOther 12 == 1
    print $ everyOther 852369 == 628
    print $ everyOther 1714 == 11
    print $ everyOther 12345 == 42
    print $ everyOther 891 == 9
    print $ everyOther 123 == 2
    print $ everyOther 2121 == 22
    print $ everyOther 4736778 == 767
    print $ everyOther 448575 == 784
    print $ everyOther 4214 == 14
    print $ everyOther 2 -- Error

everyOther :: Int -> Int 
everyOther n
    | n < 10 = error "n must be >= 10 (n >= 10)"
    | otherwise = acc n 0 1
        where
            acc :: Int -> Int -> Int -> Int
            acc 0 new count = new
            acc n new count
                | mod count 2 == 0 = acc (div n 10) (new * 10 + mod n 10) (count + 1)
                | otherwise        = acc (div n 10) new (count + 1)