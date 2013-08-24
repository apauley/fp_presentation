import System.Environment
import Data.List

main :: IO()
main = do
    args <- getArgs
    let result = do_something_with_args args
    putStrLn result

do_something_with_args args = intercalate "-" args
