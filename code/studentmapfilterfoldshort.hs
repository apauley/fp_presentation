data Student = Student { firstName      :: String
                       , lastName       :: String
                       , finalExamScore :: Double}

instance Show Student where
         show (Student firstName lastName score) = unwords [firstName, lastName, "("++ show score ++ ")"]

allscores = map finalExamScore

passed = filter has_passed
has_passed student = finalExamScore student >= 60

namecat = foldl catfun ""
        where catfun = (\acc student ->
                     acc ++ (firstName student) ++ "\n")

students = [Student {firstName="John",  lastName="Deer",    finalExamScore=60},
            Student {firstName="Billy", lastName="Bob",     finalExamScore=49.1},
            Student {firstName="Jane",  lastName="Doe",     finalExamScore=89},
            Student {firstName="Jack",  lastName="Johnson", finalExamScore=29.3}]

main :: IO()
main = do
    putStrLn $ "Final Exam Scores: " ++ show (allscores students)
    putStrLn $ "\nStudents that have passed:\n" ++ show (passed students)
    putStrLn $ "\nStudent names:\n" ++ show (namecat students)
