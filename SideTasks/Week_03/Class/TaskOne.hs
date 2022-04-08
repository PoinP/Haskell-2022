import Distribution.Compat.CharParsing (digit)
-- A narcissistic number is a number which is the sum of its own digits, 
-- each raised to the power of the number of digits. Define a predicate 
-- that checks whether a given whole number is a narcissistic number.

-- Implementation detail: Use only linearly recursive procedures.
-- For example, take 153 (3 digits):
-- 1^3 + 5^3 + 3^3 = 1 + 125 + 27 = 153

-- and 1634 (4 digits):
-- 1^4 + 6^4 + 3^4 + 4^4 = 1 + 1296 + 81 + 256 = 1634

main :: IO()
main = do
    print $ isNarcissistic 7 == True
    print $ isNarcissistic 12 == False
    print $ isNarcissistic 370 == True
    print $ isNarcissistic 371 == True
    print $ isNarcissistic 1634 == True

getDigits :: Int -> Int
getDigits x = acc x 0
    where
        acc :: Int -> Int -> Int
        acc 0 digits = digits
        acc x digits = acc (div x 10) (digits + 1)

isNarcissistic :: Int -> Bool
isNarcissistic x = x == acc x (getDigits x)
    where
        acc 0 digits = 0
        acc n digits = mod n 10 ^ digits + acc (div n 10) digits