data Student = Student { firstName      :: String
                       , lastName       :: String
                       , finalExamScore :: Double}

instance Show Student where
         show (Student firstName lastName score) = unwords [firstName, lastName, "("++ show score ++ ")"]

passed :: [Student] -> [Student]
passed students = filter has_passed students

has_passed :: Student -> Bool
has_passed student = finalExamScore student >= 60

students = [Student {firstName="John",  lastName="Deer",    finalExamScore=60},
            Student {firstName="Billy", lastName="Bob",     finalExamScore=49.1},
            Student {firstName="Jane",  lastName="Doe",     finalExamScore=89},
            Student {firstName="Jack",  lastName="Johnson", finalExamScore=29.3}]

main :: IO()
main = do
    putStrLn $ "Students that have passed:\n" ++ show (passed students)
