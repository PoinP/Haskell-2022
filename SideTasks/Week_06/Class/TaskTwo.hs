-- Define a function that accepts two unary functions "f" and "g" and a
-- list of values and checks whether f dominates g. We say that one function
-- dominates another if for every value the absolute value after applying "f" 
-- is greater than or equal to the absolute value after applying "g".

-- Implementation detail: Solve the task with one line of code using folding.

main :: IO()
main = do
    print $ dominates (\x -> x + 1) (\x -> x + 2) [1, 2, 3, 4, 5] == False
    print $ dominates (\x -> x * 3) (\x -> x + 2) [1, 2, 3, 4, 5] == True

dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates f g xs = foldr (\x acc -> acc + f x) 0 xs >= foldr (\x acc -> acc + g x) 0 xs