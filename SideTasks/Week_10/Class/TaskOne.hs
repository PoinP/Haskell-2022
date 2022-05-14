-- Define a new polymorphic algrebraic data type called Point.
-- A point may have two or three dimensions. Create an instance for every dimension and print it.

main :: IO()
main = do
    print $ Point2D 2 2
    print $ Point3D 2 2 2

data Point a = Point2D a a | Point3D a a a
    deriving(Show)