-- Дефинирайте функция repeater str, която получава като аргумент символен низ и връща 
-- анонимна функция на два аргумента - count и glue (число и низ). 
-- Оценката на обръщението към върнатата функция е низ, който се получава 
-- чрез count-кратно повтаряне на низа str, при което между всеки две съседни \
-- повторения на str стои низът glue.

-- Implementation detail: count will always be a natural number. You needn't validate it!

main :: IO()
main = do
    print $ (repeater "I love Haskell") 3 " " == "I love Haskell I love Haskell I love Haskell"
    print $ (repeater "Quack") 5 "!" == "Quack!Quack!Quack!Quack!Quack"

repeat' :: [a] -> [a] -> Int -> [a]
repeat' str glue 0 = []
repeat' str glue 1 = str
repeat' str glue n = str ++ glue ++ repeat' str glue (n - 1)

repeater :: [Char] -> (Int -> [Char] -> [Char])
repeater str = (\count glue -> repeat' str glue count)