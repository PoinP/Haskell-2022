main :: IO()
main = do
    print $ stocklist stocks ['A','B'] == [('A',200),('B',1140)]
    print $ stocklist stocks ['C','X'] == [('C',500),('X',0)]
    print $ stocklist stocks ['Y','X'] == [('Y',0),('X',0)]
    print $ stocklist stocks ['C'] == [('C', 500)]

data Stock = Stock String Int

stocks :: [Stock]
stocks = [Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600]

stocklist :: [Stock] -> [Char] -> [(Char, Int)]
stocklist s l = [(l', sum [p | (Stock c p) <- s, c !! 0 == l']) | l' <- l]