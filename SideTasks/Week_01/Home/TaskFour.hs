-- Define a function for calculating the GCD of two whole numbers.

-- Note: Normally, you would use the built-in function gcd.

--      solve using guards;
--      solve using pattern matching; <- The preferred way!

main :: IO()
main = do
    print $ myGcdG 5 13 == 1
    print $ myGcdG 13 1235 == 13

    print $ myGcdPM 5 13 == 1
    print $ myGcdPM 13 1235 == 13

myGcdG :: Int -> Int -> Int
myGcdG x y
    | mod (max y x) (min x y) == 0 = x
    | otherwise = myGcdG ((min x y) - 1) (max x y)

myGcdPM :: Int -> Int -> Int
myGcdPM a 0 = a
myGcdPM a b = myGcdPM b (mod a b)