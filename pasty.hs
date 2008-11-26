
import System.Environment


main :: IO ()
main = 
 
  do
    commandLine <- getArgs
    let fileNames = parse_args commandLine



{-|
  parse the command line and extract filenames and which columns 
  to extract from each of the files
-}
parse_args :: [String] -> [String]
parse_args args = args
