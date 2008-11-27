{-----------------------------------------------------------------
 
  (c) 2008 Markus Dittrich 
 
  This program is free software; you can redistribute it 
  and/or modify it under the terms of the GNU General Public 
  License Version 3 as published by the Free Software Foundation. 
 
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License Version 3 for more details.
 
  You should have received a copy of the GNU General Public 
  License along with this program; if not, write to the Free 
  Software Foundation, Inc., 59 Temple Place - Suite 330, 
  Boston, MA 02111-1307, USA.

--------------------------------------------------------------------}

-- imports
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import System.IO
import System.IO.Error
import System.Environment
import Debug.Trace

-- local imports
import ByteStringHelper

main :: IO ()
main = 
 
  do
    commandLine <- getArgs
    let parsedCommands = parse_args commandLine
    case parsedCommands of
      Nothing -> print_usage 
      Just (selections,files) -> do
          contents <- read_files [] files
          let output = paste_columns selections contents
          mapM_ B.putStrLn output 


{-|
  paste the selected contents of all files
  NOTE: for now we assume an arbitrary number of spaces
        as column separators
-}
paste_columns :: [ColumnList] -> [B.ByteString] -> [B.ByteString]
paste_columns _ []  = []
paste_columns [] xs =

  let 
    sortedSplitByLine       = split_lines xs
    --sortedSplitByLine = sortBy sort_by_length splitByLine 
  in
    walk_rows (head sortedSplitByLine) (tail sortedSplitByLine)

  where
    walk_rows :: [B.ByteString] -> [[B.ByteString]] -> [B.ByteString]
    --walk_rows [] _      = []
    walk_rows x []     = x
    walk_rows x (y:ys) = walk_rows (merge [] x y) ys

    
    merge :: [B.ByteString] -> [B.ByteString] -> [B.ByteString] 
          -> [B.ByteString]
    merge acc []     _      = reverse acc
    merge acc (i:is) []     = merge (i:acc) is []
    merge acc (i:is) (j:js) = merge (B.append i j:acc) is js

{-|
  ordering function for list sorting by length, 
  longest one first
-}
sort_by_length :: [a] -> [a] -> Ordering
sort_by_length x y 
  | length x == length y   = EQ
  | length x >  length y   = LT
  | otherwise              = GT


{-|
  split a ByteString into its constituent lines, i.e., bytes
  separated by a newline '\n'
-}
split_lines :: [B.ByteString] -> [[B.ByteString]]
split_lines = map (B.split newLineW) . equilize 


{-|
  read all files provided on command line and store contents
  in a ByteString
-}
read_files :: [B.ByteString] -> [String] -> IO [B.ByteString]
read_files acc []     = return $ reverse acc
read_files acc (x:xs) = 
  
  do
    content <- try_read_file x
    read_files (content:acc) xs


{-| 
  try to open the given file. If things fail try to detect
  why and provide the user with some sort of useful error 
  message (generic one otherwise); then quit 
-}
try_read_file :: FilePath -> IO B.ByteString
try_read_file path = do
  content <- try (B.readFile path)

  case content of
    Right fileContent -> return fileContent
    Left e            -> do
        let reason = analyze_error e
        putStrLn $ "\nERROR: Cannot parse file " ++ path ++ ": "
                 ++ reason 
        error "... aborting."

      where
        analyze_error :: IOError -> String
        analyze_error err
          | isPermissionError err   = "Permission denied."
          | isDoesNotExistError err = "File does not exist."
          | otherwise               = "Unknown problem :("



{-|
  parse the command line and extract filenames and which columns 
  to extract from each of the files
-}
parse_args :: [String] -> Maybe ([ColumnList],[String])
parse_args []  =  Nothing
parse_args a@(x:y:xs) 
  | x == "-n"  = Just (parse_column_specs y, xs)
  | otherwise  = Just ([],a)
parse_args s = Just ([],s)



{-| 
  list of columns to be extracted for a particular file
-}
type ColumnList = [Int]


{-|
  parses the user specified list of columns to be extracted for
  each file
-}
parse_column_specs :: String -> [ColumnList]
parse_column_specs x = 
  
  let
    perFile = parse_per_file_columns [] x
  in
    map (parse_column []) perFile

  where

    {- parse full item into per file items separated by : 
       since we parse recursively, make sure to reverse 
       the final list -}
    parse_per_file_columns :: [String] -> String -> [String]
    parse_per_file_columns acc []  = reverse acc
    parse_per_file_columns acc allItems = 

      let
        (next,raw_rest) = break ( == ':' ) allItems
        rest = strip_leading_char ':' raw_rest
      in
        parse_per_file_columns (next:acc) rest


    {- parse individual file items separated by , -}
    parse_column :: ColumnList -> String -> ColumnList
    parse_column acc []  = sort acc
    parse_column acc fileItems =

      let
        (next,raw_rest) = break ( == ',' ) fileItems
        rest = strip_leading_char ',' raw_rest
      in
        parse_column (read next:acc) rest


{-|
  strip leading type of char from string in case there is one
-}
strip_leading_char :: Char -> String -> String
strip_leading_char _ [] = []
strip_leading_char c a@(x:xs)   
  | x == c  = xs
  | otherwise = a


{-|
  print usage information for pasty
-}
print_usage :: IO ()
print_usage = 
  putStrLn "Usage: pasty [-n a1,a2,...[:b1,b2,..]] file1 file2"  

