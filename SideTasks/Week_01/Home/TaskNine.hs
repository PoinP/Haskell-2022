-- Define a function that reverses a non-negative number by implementing a linearly iterative process.

main :: IO()
main = do
    print $ rev 1 == 1
    print $ rev 123 == 321
    print $ rev 987654321 == 123456789
    print $ rev 827266127 == 721662728

rev :: Int -> Int
rev n = acc n 0 where
    acc :: Int -> Int -> Int
    acc 0 res = res
    acc x res = acc (div x 10) (res * 10 + (mod x 10))