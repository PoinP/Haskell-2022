-- Define predicates that check whether a list has at least one element.

-- Implementation details:

-- - Solve using pattern matching.
-- - Solve using functions.
-- - Solve using a function defined at a functional level.

main :: IO()
main = do
    print $ hasElementsPM [] == False
    print $ hasElementsPM [1, 2, 3] == True

    print $ hasElementsFunc [] == False
    print $ hasElementsFunc [1, 2, 3] == True

    print $ hasElementsButWithMagic [] == False
    print $ hasElementsButWithMagic [1, 2, 3] == True

hasElementsPM :: [Int] -> Bool
hasElementsPM [] = False
hasElementsPM xs  = True 

hasElementsButWithMagic :: [Int] -> Bool
hasElementsButWithMagic xs = not(null xs)

hasElementsFunc :: [Int] -> Bool
hasElementsFunc xs = length xs /= 0