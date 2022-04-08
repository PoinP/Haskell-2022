-- Define the Vector data type that is the tuple (x, y, z). Define the following functions:

-- sumVectors - returns the sum of two vectors;
-- scaleVector - returns a vector scaled by a number.
-- Implementation detail: Use the type annotation!

main :: IO()
main = do
    print $ sumVectors (1, 2, 3) (4, 5, 6) == (5, 7, 9)
    print $ sumVectors (0, 0, 0) (100, 200, -300) == (100, 200, -300)

    print $ scaleVector (1, 2, 3) 5 == (5, 10, 15)
    print $ scaleVector (5, 2, 159) (-2) == (-10, -4, -318)

type Vector a = (a, a, a)

sumVectors :: Num a => Vector a -> Vector a -> Vector a
sumVectors (x, y, z) (a, b, c) = (x + a, y + b, z + c)

scaleVector :: Num a => Vector a -> a -> Vector a
scaleVector (x, y, z) a = (x * a, y * a, z * a)