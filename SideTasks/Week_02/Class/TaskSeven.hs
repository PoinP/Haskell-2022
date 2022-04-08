-- Define a predicate that checks whether the digits of a non-negative whole number are ordered in an ascending order.

main :: IO()
main = do 
    print $ hasIncDigits 1244 == True
    print $ hasIncDigits 12443 == False
    print $ hasIncDigits 789 == True 
    print $ hasIncDigits 781 == False
    print $ hasIncDigits (-652) == False --Error

hasIncDigits :: Int -> Bool 
hasIncDigits 0 = True 
hasIncDigits x
    | x < 0 = error "Must pass a non-negative number!"
    | mod x 10 < div (mod x 100) 10 = False
    | otherwise = hasIncDigits (div x 10)