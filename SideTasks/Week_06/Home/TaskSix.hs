-- Define a function that accepts a list of whole number one-argument functions
-- [f1, f2 .. fn] and returns a function that for every x calculates the composition
--  of the functions with odd indices: f1(f3(...(fnx)...)).

-- Implementation detail: Solve the task with one line of code using folding.

main :: IO()
main = do
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2

getOddCompositionValue :: [Int -> Int] -> Int -> Int
getOddCompositionValue fs = fst (foldr (\f (g, c) -> if odd c then (g . f, c + 1) else (g, c + 1)) (id, 0) fs)