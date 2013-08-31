data Student = Student { firstName      :: String
                       , lastName       :: String
                       , finalExamScore :: Double}
                       deriving Show

allscores :: [Student] -> [Double]
allscores = map finalExamScore

students = [Student {firstName="John",
                     lastName="Deer",
                     finalExamScore=60},
            Student {firstName="Billy",
                     lastName="Bob",
                     finalExamScore=49.1},
            Student {firstName="Jane",
                     lastName="Doe",
                     finalExamScore=89},
            Student {firstName="Jack",
                     lastName="Johnson",
                     finalExamScore=29.3}]

main :: IO()
main = do
    putStrLn $ "Final Exam Scores: " ++ show (allscores students)
