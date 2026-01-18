module Main (main) where

import DosyearLeapCheck (DosyearError (..), str2isLeap)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

showLower :: Bool -> String
showLower True = "true"
showLower False = "false"

result2logStr :: Either DosyearError Bool -> String
result2logStr (Right b) = "severity:info\tis_leap:" ++ showLower b
result2logStr (Left (InvalidYearString s)) = "severity:error\trejected_string:" ++ s ++ "\tdetail:invalid year string"
result2logStr (Left (NegativeRelativeYear i)) = "severity:warn\trejected_number:" ++ show i ++ "\tdetail:year out of range"
result2logStr (Left (RelativeYearOutOfRange i)) = "severity:warn\trejected_number:" ++ show i ++ "\tdetail:year out of range"

main :: IO ()
main = do
    yearStr <- getLine
    let result = str2isLeap yearStr
    hPutStrLn stderr (result2logStr result)
    case result of
        Right True -> exitSuccess
        _ -> exitFailure
