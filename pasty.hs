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
--import Debug.Trace

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
paste_columns :: ColumnList -> [B.ByteString] -> [B.ByteString]
paste_columns _ []      = []
paste_columns colList xs 
  | colList == []  = walk_full_rows (head splitLines) (tail splitLines)
  | otherwise      = walk_itemized_rows (tail colList) 
                       itemizedFirstRow (tail splitLines) 

  where
    splitLines       = split_into_lines xs
    itemizedFirstRow = prepare_first_row [] (head splitLines) 
                        (head colList)

    {- for each file, provided on the command line walk through all
       rows and append them completely -}
    walk_full_rows :: [B.ByteString] -> [[B.ByteString]] 
                      -> [B.ByteString]
    walk_full_rows acc []     = acc
    walk_full_rows acc (y:ys) = walk_full_rows (merge_full_row [] 
                                  acc y) ys
   

    {- for each file, provided on the command line walk through all
       rows and append the request items -}
    walk_itemized_rows :: ColumnList -> [B.ByteString] 
                          -> [[B.ByteString]] -> [B.ByteString]
    walk_itemized_rows _ acc []     = acc
    walk_itemized_rows [] acc _     = acc
    walk_itemized_rows (c:cs) acc (y:ys) = 
      walk_itemized_rows cs (merge_row_items c [] acc y) ys


    {- for each file, append complete rows -}
    merge_full_row :: [B.ByteString] -> [B.ByteString] 
                       -> [B.ByteString] -> [B.ByteString]
    merge_full_row acc [] _          = reverse acc
    merge_full_row acc _  []         = reverse acc
    merge_full_row acc (l:ls) (r:rs) = 
      merge_full_row (cat_columns l r:acc) ls rs


    {- prepare the first columns of the first file according
       to the user selection -}
    prepare_first_row :: [B.ByteString] -> [B.ByteString] -> [Int] 
                         -> [B.ByteString]
    prepare_first_row acc [] _ = reverse acc
    prepare_first_row acc (l:ls) cs = 
      prepare_first_row (cat_column_items B.empty l cs:acc) ls cs 


    {- for each file, append the requested items of each row -} 
    merge_row_items :: ColumnSpec -> [B.ByteString] -> [B.ByteString] 
                       -> [B.ByteString] -> [B.ByteString]
    merge_row_items _ acc [] _     = reverse acc
    merge_row_items _ acc _ []     = reverse acc
    merge_row_items [] acc _ _     = reverse acc
    merge_row_items c acc (l:ls) (r:rs) = 
      merge_row_items c (cat_column_items l r c:acc) ls rs
      


{-|
  concatenate two column entries consisting of ByteStrings
  and an intervening space to give a new row
-}
cat_columns :: B.ByteString -> B.ByteString -> B.ByteString
cat_columns x y = B.append x $ B.append space y


{-|
  concatenate the first column entry with the requested items of
  the second column separated by a space to give the new column 
-}
cat_column_items :: B.ByteString -> B.ByteString -> ColumnSpec
                    -> B.ByteString
cat_column_items x _ []     = x
cat_column_items x y c      =

  let
    rowItems   = split_into_items y
    validSpecs = filter ( < length rowItems) c
  in
    cat_items x rowItems validSpecs

    where
      cat_items :: B.ByteString -> [B.ByteString] -> [Int] 
                   -> B.ByteString
      cat_items l _ []      = l
      cat_items l xs (z:zs) = cat_items (cat_columns l $ xs !! z) xs zs


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
parse_args :: [String] -> Maybe (ColumnList,[String])
parse_args []  =  Nothing
parse_args a@(x:y:xs) 
  | x == "-n"  = Just (parse_column_specs y xs, xs)
  | otherwise  = Just ([],a)
parse_args s = Just ([],s)


{-| 
  Map, providing a mapping of each file to a list of columns to
  paste 
-}
type ColumnSpec = [Int]
type ColumnList = [ColumnSpec]


{-|
  parses the user specified list of columns to be extracted for
  each file
-}
parse_column_specs :: String -> [String] -> ColumnList
parse_column_specs colSpec files = 
  
  let
    perFile = parse_per_file_columns [] colSpec
    columnSelection = map (parse_column []) perFile
  in
    pad_column_list columnSelection (length files)

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
    parse_column :: ColumnSpec -> String -> ColumnSpec
    parse_column acc []  = sort acc
    parse_column acc fileItems =

      let
        (next,raw_rest) = break ( == ',' ) fileItems
        rest = strip_leading_char ',' raw_rest
      in
        parse_column (read next:acc) rest


    {- pad (or trims) the ColumnList to make sure we have the
       same number of elements than we have files -}
    pad_column_list :: ColumnList -> Int -> ColumnList
    pad_column_list [] _ = []
    pad_column_list xs numFiles 
      | length xs >= numFiles   = take numFiles xs
      | otherwise               = 

        let
          reverseList = reverse xs
          pading      = take (numFiles - length xs) reverseList
        in
          reverse ( pading ++ reverseList)


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

