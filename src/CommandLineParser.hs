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
  CommandLineParser provides all tools needed to parse the
  command line
-}
module CommandLineParser ( parse_args
                         , print_usage
                         ) where

-- imports
import Data.List

-- local imports
import Parser



{-|
  parse the command line and extract filenames and which columns 
  to extract from each of the files.
  If we encounter a something that looks like an option (i.e.
  starts with a dash) and we don't know it we return Nothing to
  force a print_usage; otherwise we'll try to read the item as a 
  file which will of course fail.
-}
parse_args :: [String] -> Maybe ([ColumnSpec],[String])
parse_args []  =  Nothing
parse_args a@(x:y:xs) 
  | (x == "-c") || (x == "--colspecs") = 
        Just (parse_column_specs y xs, xs)
  | head x == '-'   = Nothing 
  | otherwise       = Just ([],a)
parse_args s        = Just ([],s)



{-|
  parses the user specified list of columns to be extracted for
  each file
-}
parse_column_specs :: String -> [String] -> [ColumnSpec]
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


    {- pad (or trims) [ColumnSpec] to make sure we have the
       same number of elements than we have files -}
    pad_column_list :: [ColumnSpec] -> Int -> [ColumnSpec]
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
print_usage = do
  putStrLn "pasty 0.0   (C) 2008 Markus Dittrich"
  putStrLn "Usage: pasty [-c | --colspecs <columnspecs>] file1 file2"  

