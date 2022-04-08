main :: IO()
main = do
    print $ splitPoints (1, 1) 5 [(1, 2), (2, 3), (10, 15), (-1, 1), (12, 14)] == ([(1.0, 2.0), (2.0, 3.0), (-1.0, 1.0)], [(10.0, 15.0), (12.0, 14.0)])
    print $ splitPoints (10, 10) 5 [(1, 2), (2, 3), (10, 15), (-1, 1), (12, 14)] == ([(10.0, 15.0), (12.0, 14.0)], [(1.0, 2.0), (2.0, 3.0), (-1.0, 1.0)])
    print $ splitPoints (0, 0) 2 [(0, 0), (1, 1), (2, 2), (0, 2)] == ([(0.0,0.0),(1.0,1.0),(0.0,2.0)],[(2.0,2.0)])
    print $ splitPoints (0, 0) (-1) [(0, 0), (1, 1), (2, 2), (0, 2)] -- Should give an error

type Point = (Double, Double)

isInCicle :: Point -> Point -> Double -> Bool
isInCicle (x1, y1) (x2, y2) r = sqrt ((x2 - x1)^ 2 + (y2 - y1) ^2) <= r

splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints p r ps 
    | ps == filter (\x -> not(isInCicle p x r)) ps = error "Error! No dots in circle!"
    | otherwise  = ([pnt | pnt <- ps, isInCicle pnt p r], [pnt | pnt <- ps, not (isInCicle pnt p r)])