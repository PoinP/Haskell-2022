-- Define a function that accepts a one-argument function f and a 
-- number y and returns a one-argument function that for every x returns:

-- y, if it is greater than or equal to the result of applying f to x;
-- the result of applying f to x, otherwise.
-- Implementation details:

-- Solve the task with one line of code!
-- Initially use Double, but then transition to all number types.

main :: IO()
main = do 
        print $ (upperBound (*2) 100) 50 == 100
        print $ (upperBound (*2) 100.236) 500.002 == 1000.004
        print $ (upperBound (\x -> x) 1.001) 1.001 == 1.001

upperBound :: Ord a => (a -> a) -> a -> (a -> a)
upperBound f y = (\x -> max (f x) y)

upperBound' :: Ord a => (a -> a) -> a -> (a -> a)
upperBound' f x y = max (f x) y