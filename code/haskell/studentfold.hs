data Student = Student { firstName      :: String
                       , lastName       :: String
                       , finalExamScore :: Double} deriving Show

students = [Student {firstName="John",  lastName="Deer",    finalExamScore=60},
            Student {firstName="Billy", lastName="Bob",     finalExamScore=49.1},
            Student {firstName="Jane",  lastName="Doe",     finalExamScore=89},
            Student {firstName="Jack",  lastName="Johnson", finalExamScore=29.3}]

namecat :: [Student] -> String
namecat students = foldl catfun "" students

catfun :: String -> Student -> String
catfun acc student = acc ++ (firstName student) ++ "\n"

main :: IO()
main = do
    putStrLn $ "Student names:\n" ++ show (namecat students)
