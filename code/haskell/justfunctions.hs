import System.Environment (getArgs)
import Data.List (intercalate)
import Data.Time (getCurrentTime, UTCTime)

main :: IO()
main = do
    time <- getCurrentTime
    args <- getArgs
    putStrLn (outputString time args)

outputString :: UTCTime -> [String] -> String
outputString time args =
    show(time) ++ " " ++ joinArgs(args)

joinArgs :: [String] -> String
joinArgs = intercalate "-"
