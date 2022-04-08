-- Write a function that takes two parameters a and b and returns the tuple (x, y) 
-- where x is the whole number division between a and b and y is their modular number division.

-- solve without pattern matching;
-- solve with pattern matching;
-- solve using a lambda.
-- Implementation detail: Initially use Int, but then transition to a new type Point.

main :: IO()
main = do
    print $ divideNonPM (10, 5) == (2, 0)
    print $ divideNonPM (5, 10) == (0, 5)

    print $ dividePM (10, 5) == (2, 0) -- 10 / 5 = 2 and 10 % 5 = 0
    print $ dividePM (5, 10) == (0, 5) -- 5 / 10 = 0 and 5 % 10 = 5

    print $ myLambda (10, 5) == (2, 0)
    print $ myLambda (5, 10) == (0, 5)

type Point = (Int, Int)

divideNonPM :: Point -> Point
divideNonPM vec = (div (fst vec) (snd vec), mod (fst vec) (snd vec))

dividePM :: Point -> Point
dividePM (x, y) = (div x y, mod x y)

myLambda :: Point -> Point
myLambda = (\(x, y) -> (div x y, mod x y))