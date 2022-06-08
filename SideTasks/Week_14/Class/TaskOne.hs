main::IO()
main = do
    print $ (getIndices [2, 7, 11, 15]) 9 == (0, 1)
    print $ (getIndices [3, 2, 4]) 6 == (1, 2)
    print $ (getIndices [3, 3]) 6 == (0, 1)

getIndices :: [Int] -> (Int -> (Int, Int))
getIndices xs = (\n -> head [(xi, yi) | (x,xi) <- zip xs [0 ..], (y,yi) <- zip xs [0 ..], xi /= yi, x + y == n])