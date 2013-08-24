import System.Environment (getArgs)
import Data.List (intercalate)

main :: IO()
main = do
    args <- getArgs
    let result = do_something_with_args args
    putStrLn result

do_something_with_args :: [[Char]] -> [Char]
do_something_with_args args = intercalate "-" args
