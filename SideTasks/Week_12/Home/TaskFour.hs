data Measuring = Temp Int Float

main :: IO()
main = do
    print $ closestToAverage [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2),
                      (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)] == 6

closestToAverage :: [Measuring] -> Int
closestToAverage ms = head [d | (Temp d t) <- ms, t == closestTemp]
    where
        ts = map (\ (Temp _ t) -> t) ms
        average = sum ts / fromIntegral (length ts)
        closestTemp = foldl1 (\t1 t2 -> if abs (t1 - average) > abs (t2 - average) then t2 else t1) ts