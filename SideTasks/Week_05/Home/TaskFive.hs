main :: IO()
main = do
    print $ pairCompose [(\x -> x + 1), (\x -> x + 2), (\x -> x + 3)] 1 == 8

pairCompose :: [Int -> Int] -> (Int ->  Int)
pairCompose [] = id
pairCompose [f] = f
pairCompose [f, g] = f . g
pairCompose (f:g:fs) = (\x -> (f.g) x + pairCompose fs x)