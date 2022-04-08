import Data.Char (digitToInt)

-- In ONE line of code:

-- Reverse a number;
-- Check whether a number is prime;
-- Returns the sum of the digits of a number;
-- Find the sum of the divisors of a number.

main :: IO()
main = do
    print $ rev 123 == 321

    print $ isPrime' 5 == True
    print $ isPrime' 6 == False
    print $ isPrime' 11 == True
    print $ isPrime' 13 == True

    print $ sumDig 142500 == 12

    print $ sumDivs 161 == 192

rev :: Int -> Int
rev = read . reverse . show

isPrime :: Int -> Bool
isPrime y = y > 1 && foldr (\x acc -> acc && mod y x /= 0) True [2 .. y - 1]

isPrime' :: Int -> Bool
isPrime' x = x > 1 && and [mod x n /= 0 | n <- [2 .. x - 1]]

isPrime'' :: Int -> Bool
isPrime'' x = x > 1 && [1, x] == [n | n <- [1 .. x], mod x n == 0]

sumDig :: Int -> Int
sumDig = foldr (\x acc -> acc + digitToInt x) 0 . show

sumDig' :: Int -> Int
sumDig'= sum . map digitToInt . show  -- Would love to discuss this one to see if I understand how it works

sumDivs :: Int -> Int
sumDivs x = sum [n | n <- [1 .. x], mod x n == 0]