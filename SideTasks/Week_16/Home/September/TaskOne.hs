main :: IO()
main = do
    print $ bestStudents [("Иван Иванов", 6.0), ("Петър Петров", 5.5), ("Мария Маринова", 6.0), ("Марина Петрова", 5.0)] == ["Иван Иванов", "Мария Маринова"]

type Name = String
type Grade = Double
type Student = (Name, Grade)

bestStudents :: [Student] -> [Name]
bestStudents lst = [n | (n, g) <- lst, g == maxGrade]
    where
        maxGrade = maximum [g | (_, g) <- lst]