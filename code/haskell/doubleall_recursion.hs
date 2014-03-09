doubleAll :: [Int] -> [Int]
doubleAll []     = []
doubleAll (x:xs) = x*2 : doubleAll xs

main :: IO()
main = do
   let doubledList = doubleAll [6,2,14]
       strList = [(show i) ++ " " | i <- doubledList]
   putStrLn $ concat strList
