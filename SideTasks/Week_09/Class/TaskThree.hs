-- Define a new data type called "Shape". Shape must have four constructors:

-- Circle: with one argument representing the radius;
-- Rectangle: with two arguments representing the width and height;
-- Triangle;
-- Cylinder with two arguments for the radius of the base and height.
-- Create a shape from every type and output it.

main :: IO()
main = do
    print $ Circle 2
    print $ Rectangle 2 2
    print $ Triangle 2 2 2

data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
    deriving (Show)