-- Create a new data type called Point2D and define a function that accepts two functions: 
-- f and g and a list of points with two coordinates. The function should return those points 
-- for which f(firstCoordinate) = g(secondCoordinate) and should be able to work with whole as 
-- well as floating point numbers!

main :: IO()
main = do
    print $ myImages (\x -> x * x) (2+) [Point 2 2, Point 1 2, Point 3 7] == [Point 2 2, Point 3 7]

data Point2D a = Point a a
    deriving (Show, Eq)

myImages :: (Int -> Int) -> (Int -> Int) -> [Point2D Int] -> [Point2D Int]
myImages f g ps = [ Point x y | (Point x y) <- ps, f x == g y]