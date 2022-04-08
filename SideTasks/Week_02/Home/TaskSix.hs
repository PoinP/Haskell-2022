-- A number is interesting if and only if it is evenly divided by the sum of its digits. 
-- Define a predicate that checks whether a number is interesting.

main :: IO()
main = do
    print $ isInteresting 410 == True
    print $ isInteresting 212 == False
    print $ isInteresting 567 == False
    print $ isInteresting 70 == True 
    print $ isInteresting 5 == True 
    print $ isInteresting 4 == True 

sumDigits :: Int -> Int 
sumDigits 0 = 0
sumDigits x = mod x 10 + sumDigits (div x 10)

isInteresting :: Int -> Bool 
isInteresting x = mod x (sumDigits x) == 0