-- Write a function that outputs the sum of the tuple (x1, x2).

-- solve without pattern matching;
-- solve with pattern matching;
-- solve using a lambda.

main :: IO()
main = do
    print $ sumTupleNonPM (4, 5) == 9
    print $ sumTupleNonPM (-5, 5) == 0

    print $ sumTuplePM (4, 5) == 9
    print $ sumTuplePM (-5, 5) == 0

    print $ myLambda (4, 5) == 9
    print $ myLambda (-5, 5) == 0
    -- lambda test case

sumTupleNonPM :: (Int, Int) -> Int
sumTupleNonPM xs = fst xs + snd xs

sumTuplePM :: (Int, Int) -> Int
sumTuplePM (x, y) = x + y

myLambda :: (Int, Int) -> Int
myLambda = (\(x, y) -> x + y)