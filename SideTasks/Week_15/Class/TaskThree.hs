main :: IO()
main = do
    print $ matching "1234" == []
    print $ matching ",[.[-],]" == [(3,5),(1,7)]
    print $ matching ",+[-.,+]" == [(2,7)]
    print $ matching "[][]" == [(0,1),(2,3)]

matching :: String -> [(Int, Int)]
matching str = acc (zip str [0 ..]) []
    where
        acc [] _ = []
        acc (('[', i):xs) list = acc xs ((i, -1):list)
        acc ((']', j):xs) ((i, -1):list) = (i, j) : acc xs list
        acc (_:xs) list = acc xs list
