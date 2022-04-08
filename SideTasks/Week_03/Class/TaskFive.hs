-- Define a predicate that accepts two non-negative inputs - 
-- x and y and checks whether x is a sub-number of y.

main :: IO()
main = do
    print $ subNum 123 5123783 == True -- x = 123, y = 5123783
    print $ subNum 0 0 == True
    print $ subNum 10 101 == True
    print $ subNum 101 101 == True
    print $ subNum 10 0 == False
    print $ subNum 1253 5123783 == False
    print $ subNum 12 0 == False

getDigits :: Int -> Int
getDigits x
    | x < 0     = error "x must be a positive integer"
    | x < 10    = 1
    | otherwise = 1 + getDigits (div x 10)

subNum :: Int -> Int -> Bool
subNum x y
    | x < 0 || y < 0                 = error "x and y must be postive integers"
    | getDigits x > getDigits y      = False
    | x == mod y (10 ^ getDigits x)  = True
    | otherwise                      = subNum x (div y 10)