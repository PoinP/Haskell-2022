-- By using the following types define a function that accepts 
-- a list of records and returns the hardest subject.

main :: IO()
main = do
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 2), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 2)] == "English"
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 5), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 5)] == "Maths"

type Subject = String
type Grade = Int

type StudentScoring = (String, Subject, Grade)

minNum :: [Int] -> Int
minNum = foldr (\x acc -> if x < acc then x else acc) x
    where x = maxBound :: Int

-- findScoring :: Int -> [StudentScoring] -> StudentScoring -- Goes from left to right
-- findScoring n [] = error "Error!"
-- findScoring n ((x, y, z):ss)
--     | n == z    = (x, y, z)
--     | otherwise = findScoring n ss

findScoring :: Int -> [StudentScoring] -> StudentScoring -- Goes from right to left, as the task asks to
findScoring n [] = error "Error!"
findScoring n ss
    | n == getGrade (last ss) = last ss
    | otherwise               = findScoring n (init ss)

getSubject :: StudentScoring -> Subject
getSubject (_, x, _) = x

getGrade :: StudentScoring -> Grade
getGrade (_, _, x) = x

hardestSubject :: [StudentScoring] -> Subject
hardestSubject ss =  getSubject $ findScoring (minNum [n | (x, y, n) <- ss]) ss