import Data.List

main :: IO()
main = do
    print $ findJudge 2 [(1, 2)] == 2
    print $ findJudge 3 [(1, 3), (2, 3)] == 3
    print $ findJudge 3 [(1, 3), (2, 3), (3, 1)] == -1
    print $ findJudge 3 [(1, 2), (2, 3)] == -1
    print $ findJudge 3 [(1, 3), (1, 4), (2, 3), (2, 4), (4, 3)] == 3

nubTuple :: ([Int], [Int]) -> ([Int], [Int])
nubTuple (xs, ys) = (nub xs, nub ys)

isTrustedByAll :: [(Int, Int)] -> [Int] -> Int -> Bool
isTrustedByAll ps ns j = length (filter (\(x, p) -> p == j && x /= j) ps) == length ns

checkForJudge :: [(Int, Int)] -> [Int] -> Int -> Bool
checkForJudge ps ns j = notElem j ns && isTrustedByAll ps ns j

findJudge :: Int -> [(Int, Int)] -> Int
findJudge n xs = foldr (\x acc -> if checkForJudge xs (fst ps) x then x else acc) (-1) (snd ps)
    where ps = nubTuple $ unzip xs