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

{-|
  this file contains the main driver routines for pasty 
-}


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
import CommandLineParser


{-|
  main driver
-}
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
paste_columns _ []  = []
paste_columns [] xs = walk_full_rows (head $ split_into_lines xs) 
  (tail $ split_into_lines xs)

paste_columns colList xs = walk_itemized_rows (tail colList) 
  itemizedFirstRow (tail $ split_into_lines xs) 

  where
    itemizedFirstRow = prepare_first_row [] 
      (head $ split_into_lines xs) (head colList)



{-| 
  for each file provided on the command line walk through all
  rows and append them completely 
-}
walk_full_rows :: [B.ByteString] -> [[B.ByteString]] -> [B.ByteString]
walk_full_rows acc []     = acc
walk_full_rows acc (y:ys) = walk_full_rows (merge_full_row [] acc y) ys
   


{-|
  for each file provided on the command line walk through all
  rows and append the request items 
-}
walk_itemized_rows :: ColumnList -> [B.ByteString] -> [[B.ByteString]]
                      -> [B.ByteString]
walk_itemized_rows _ acc []          = acc
walk_itemized_rows [] acc _          = acc
walk_itemized_rows (c:cs) acc (y:ys) = 
  walk_itemized_rows cs (merge_row_items c [] acc y) ys



{-| 
  for each file append complete rows 
-}
merge_full_row :: [B.ByteString] -> [B.ByteString] -> [B.ByteString] 
                  -> [B.ByteString]
merge_full_row acc [] _          = reverse acc
merge_full_row acc _  []         = reverse acc
merge_full_row acc (l:ls) (r:rs) = 
  merge_full_row (cat_columns l r:acc) ls rs



{-|
  extract the appropriate columns of the first file on the 
  command line according to the user selection 
-}
prepare_first_row :: [B.ByteString] -> [B.ByteString] -> [Int] 
                     -> [B.ByteString]
prepare_first_row acc [] _      = reverse acc
prepare_first_row acc (l:ls) cs = 
  prepare_first_row (cat_column_items B.empty l cs:acc) ls cs 



{-|
  for each file, append the requested items of each row 
-} 
merge_row_items :: ColumnSpec -> [B.ByteString] -> [B.ByteString] 
                   -> [B.ByteString] -> [B.ByteString]
merge_row_items _ acc [] _          = reverse acc
merge_row_items _ acc _ []          = reverse acc
merge_row_items [] acc _ _          = reverse acc
merge_row_items c acc (l:ls) (r:rs) = 
  merge_row_items c (cat_column_items l r c:acc) ls rs



{-|
  concatenate the first column entry with the requested items of
  the second column separated by a space to give the new column 
-}
cat_column_items :: B.ByteString -> B.ByteString -> ColumnSpec
                    -> B.ByteString
cat_column_items acc _ []        = acc
cat_column_items acc row colSpec = cat_items acc rowItems validSpecs

  where
    {- split row into items and filter all user requested 
       columns beyong the length of the line -}
    rowItems   = split_into_items row
    validSpecs = filter ( < length rowItems) colSpec

    {- concatenate all valid columns from colSpec requested by 
       the user -}
    cat_items :: B.ByteString -> [B.ByteString] -> [Int] 
                 -> B.ByteString
    cat_items acc1 _ []      = acc1 
    cat_items acc1 items (z:zs) = 
      cat_items (cat_columns acc1 $ items !! z) items zs


{-|
  concatenate two column entries consisting of ByteStrings
  and an intervening space (unless the "left" item is an empty
  bytestring) to give a new row;
-}
cat_columns :: B.ByteString -> B.ByteString -> B.ByteString
cat_columns left right 
  | left == B.empty  = right
  | otherwise        = B.append left $ B.append space right



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



