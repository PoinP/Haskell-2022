-- By using the "Shape" data type, define two functions that accept a list of shapes and:

-- 1. The first returns their areas;
-- 2. The second returns the shape with the biggest area. !! SOLVE USING FOLDING !!

main :: IO()
main = do
    print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.53981633974483,11.25,113.30000000000001,9.127927385194024,6283.185307179587]
    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == Cylinder 20.0 30.0


data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
    deriving (Show, Ord, Eq)

perimeter :: (Num a, Floating a) => Shape a -> a
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle a b) = 2 * a + 2 * b
perimeter (Triangle a b c) = a + b + c
perimeter (Cylinder r h) = 4 * r + 2 * h

area :: (Num a, Floating a) => Shape a -> a
area (Circle r) = pi * r ^ 2
area (Rectangle a b) = a * b
area (Triangle a b c) = let p = perimeter (Triangle a b c) / 2 in sqrt (p * (p - a) * (p - b) * (p - c))
area (Cylinder r h) = 2 * pi * r * h + 2 * area (Circle r)

getAreas :: (Num a, Floating a) => [Shape a] -> [a]
getAreas = map area

maxArea :: (Num a, Floating a, Ord a) => [Shape a] -> Shape a
maxArea = foldr1 (\s1 s2 -> if area s1 > area s2 then s1 else s2)