-- Define functions that return:

-- the smaller of two whole numbers:
-- add a test with negative numbers;
--     using if-else;
--     using guards;
--     using a built-in function.
-- the last digit of a number
-- the quotient of the division of two whole numbers
-- the quotient and remainder of the division of two whole numbers
-- a whole number without its last digit
-- the quotient and remainder of the division of two real numbers
-- the quotient of the division of two real numbers
-- the average of two whole numbers
-- the number rounded to the second digit after the "."

main :: IO()
main = do
    print $ min 5 6 == 5
    print $ minIf (-5) (-6) == (-6)
    print $ minIf 15 60 == 15
    print $ minIf 60 15 == 15
    print $ minGuard 15 60 == 15
    print $ minGuard 60 15 == 15
    print $ minBuiltIn 60 15 == 15
    
    print $ lastDigit 154 == 4

    print $ quotientWhole 64 2 == 32

    print $ divWhole 154 17 == 9.058823529411764

    print $ removeLastDigit 154 == 15  

    print $ divReal 154.451 10.01 == 15.42967032967033
    print $ quotientReal 154.21 17.17 == 9
    print $ avgWhole 5 1542 == 773.5
    --print $ roundTwoDig 3.1413465345321 == 3.14
    --print $ roundTwoDigButWithMagic 3.1413465345321 == 3.14 -- partial function application and composition (defining a function at functional level)

minIf :: Int -> Int -> Int
minIf x y = 
    if x <= y
        then x
        else y

minGuard :: Int -> Int -> Int
minGuard x y
  | x <= y    = x
  | otherwise = y

minBuiltIn :: Int -> Int -> Int
minBuiltIn x y = min x y

lastDigit :: Int -> Int
lastDigit x = mod x 10

quotientWhole :: Int -> Int -> Int
quotientWhole x y = div x y

divWhole :: Double -> Double -> Double
divWhole x y = x / y

removeLastDigit :: Int -> Int
removeLastDigit x = div x 10

divReal :: Double -> Double -> Double
divReal x y = x / y

quotientReal :: Double -> Double -> Int
quotientReal x y = round (x / y)

avgWhole :: Double -> Double -> Double
avgWhole x y = (x + y) / 2

roundTwoDig :: Double -> Double
roundTwoDig x = (fromIntegral $ round $ x * 100) / 100

roundCustom :: Double -> Int -> Double
roundCustom x y
    | y < 0     = x
    | y == 0    = fromIntegral $ round $ x
    | otherwise =
        let z = fromIntegral $ 10 ^ y in 
            (fromIntegral $ round $ x * z) / z