-- Define a function that accepts an unary function and a 
-- list of natural numbers. The return value should be a 
-- function that accepts a predicate and returns the sum of 
-- the squares of the numbers which squared pass the predicate.

main :: IO()
main = do
    print $ (specialSum (5-) [1..10]) (> 0) == 30
    print $ (specialSum (+1) [(-5)..5]) odd == 45

specialSum :: (Int -> Int) -> [Int] -> (Int -> Bool) -> Int
specialSum f xs p = foldl (\acc x -> if p (f x) then f x ^ 2 + acc else acc) 0 xs