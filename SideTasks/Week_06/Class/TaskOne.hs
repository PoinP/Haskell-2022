-- Define a function that accepts a unary function and a list of 
-- numbers [y1, y2 .. yn] and returns a function that for every x 
-- calculates the expression f(y1 x) + 2 f(y2 x) + .. + n f(yn x).

-- Implementation detail: Solve the task with one line of code!

main :: IO()
main = do
    print $ (myPolynomial (\x -> x - 2) []) 5 == 0
    print $ (myPolynomial (\x -> x + 10) [3.62, 6.12, 9.45, 8.02, 5, 2]) (-5) == -356.45
    print $ (myPolynomial (\x -> x - 2) [1, 4, 7, 8, 5, 2]) 5 == 453

myPolynomial :: (Floating a, Enum a) => (a -> a) -> [a] -> (a -> a)
myPolynomial f ys = (\x -> sum [n * f (y * x) | (y, n) <- zip ys [1 ..]])