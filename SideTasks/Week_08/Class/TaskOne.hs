main :: IO()
main = do
    print $ checkSequence [2, 9, 15] == True
    print $ checkSequence [11, 14, 20, 27, 31] == True
    print $ checkSequence [11, 14, 28, 27, 31] == False
    print $ checkSequence [11, 14, 14, 29, 31] == False

checkSequence :: [Int] -> Bool
checkSequence []       = True
checkSequence [an]      = True
checkSequence (ai:aj:an) = ai < aj && div aj ai /= 0 && checkSequence (aj:an)