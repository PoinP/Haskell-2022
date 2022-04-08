import Data.List

-- Define a function that:

-- accepts a function and returns a unary lambda that applies that function to its argument;
-- accepts a predicate and returns a unary lambda that applies the negated predicate to its argument;
-- accepts two functions and returns their composition over an argument;
-- returns a function that is the partial application of f over x;

-- (\ x y z -> x y z) (\ x y -> x + y) 5 6 <- What will the output be?
-- Implementation detail: Use type annotations!

main :: IO()
main = do
    print $ (myLambda (\ x -> x)) 5 == 5
    print $ (myLambda (\ x -> x)) "Hello" == "Hello"
    print $ (myLambda (+1)) 5 == 6

    print $ (negatePred (\x -> mod x 2 == 0)) 2 == False

    print $ (compose (\x -> x - 5) (\y -> y + 25)) 5 == 25
    print $ (compose group sort) "Hello World" == [" ","H","W","d","e","lll","oo","r"]

    print $ (partiallyApply (\x y -> 10 * x + y) 5) 10 == 60

myLambda :: (a -> a) -> a -> a
myLambda f = f

negatePred :: (a -> Bool) -> a -> Bool
negatePred f x = not(f x)

compose :: (a -> c) -> (b -> a) -> b -> c   -- Why did we use diferenet types?
compose f g = f . g

partiallyApply :: (a -> a -> a) -> a -> a -> a
partiallyApply f = f