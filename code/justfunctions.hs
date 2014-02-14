import System.Environment (getArgs)
import Data.List (intercalate)
import Data.Time (getCurrentTime)

main :: IO()
main = do
    args      <- getArgs
    timestamp <- getCurrentTime
    let result = show timestamp ++ " " ++ join_args args
    putStrLn result

join_args :: [String] -> String
join_args args = intercalate "-" args
