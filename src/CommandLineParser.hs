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
import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.Word
--import Debug.Trace

-- local imports
import ByteStringHelper
import PastyData


{-|
  parse the command line and extract filenames and which columns 
  to extract from each of the files.
  If we encounter a something that looks like an option (i.e.
  starts with a dash) and we don't know it we return Nothing to
  force a print_usage; otherwise we'll try to read the item as a 
  file which will of course fail.
-}
parse_args :: [String] -> Maybe (OutputSpec, [String])
parse_args []   = Nothing
parse_args args = 

  let
    (colSpecs,rem1)     = check_option_arg parse_column_specs [] 
                            "-c" "--colspecs" args
    (outSep,rem2)       = check_option_arg BC.pack space 
                            "-u" "--outsep" rem1
    (inSep, rem3)       = check_option_arg extract_word8 spaceW
                            "-i" "--insep" rem2
    (pasteSpecs, files) = check_option_arg parse_paste_specs []
                            "-p" "--pastespecs" rem3
    colSpecsPadded      = pad_column_list colSpecs $ length files
  in
    -- check if list of files contains items starting with a dash
    -- which most likely indicated an invalid command line option
    if check_for_invalid_opts files 
      then
        Nothing
      else
        Just ( defaultOutputSpec { parseSpec = colSpecsPadded
                                 , pasteSpec = pasteSpecs
                                 , inputSep   = inSep
                                 , outputSep  = outSep 
                                 }
             , files)


  where
    -- generic parse function for option without optstring
    check_option :: String -> String -> [String] -> (Bool,[String])
    check_option = check_option_h []

      where
        check_option_h :: [String] -> String -> String -> [String] 
                          -> (Bool,[String])
        check_option_h acc _ _ [] = (False, reverse acc)
        check_option_h acc shortOp longOp (x:xs)
          | (x == shortOp) || (x == longOp) = (True, reverse acc ++ xs)
          | otherwise = check_option_h (x:acc) shortOp longOp xs


    -- generic parse function for option with mandatory optstring
    check_option_arg :: (String -> a) -> a -> String -> String 
                    -> [String] -> (a,[String])
    check_option_arg = check_option_arg_h []

      where
        check_option_arg_h :: [String] -> (String -> a) -> a -> String 
                              -> String -> [String] -> (a,[String])
        check_option_arg_h acc _ def _ _ []     = (def, reverse acc)
        check_option_arg_h acc f def shortOp longOp (x:y:zs)    
          | (x == shortOp) || (x == longOp )  = 
                (f y, reverse acc ++ zs)
          | otherwise = check_option_arg_h (x:acc) f def shortOp 
                          longOp $ y:zs

        check_option_arg_h acc f def shortOp longOp (x:xs) = 
              check_option_arg_h (x:acc) f def shortOp longOp xs


    -- check final list of files for any potentially invalid
    -- command line options
    check_for_invalid_opts :: [String] -> Bool
    check_for_invalid_opts [] = False
    check_for_invalid_opts (x:xs)
      | head x == '-'         = True
      | otherwise             = check_for_invalid_opts xs


    -- extract a Word8 from a String passed on the command line
    -- if the string has more than a single char we simple pick
    -- the first one for now; if the string is empty we take the
    -- default
    extract_word8 :: String -> Word8
    extract_word8 item 
      | length item == 0    = spaceW
      | otherwise           = char_to_Word8 $ head item



{-|
  parses the user specified list providing the order in with
  the parsed columns are to be punched
-}
parse_paste_specs :: String -> ColumnSpec
parse_paste_specs = parse_column []



{-|
  parses the user specified list of columns to be extracted for
  each file
-}
parse_column_specs :: String -> [ColumnSpec]
parse_column_specs colSpec = 
  
  let
    perFile = parse_per_file_columns [] colSpec
  in
    map (sort . parse_column []) perFile

  where

    -- parse full item into per file items separated by : 
    -- since we parse recursively, make sure to reverse 
    -- the final list
    parse_per_file_columns :: [String] -> String -> [String]
    parse_per_file_columns acc []  = reverse acc
    parse_per_file_columns acc allItems = 

      let
        (next,raw_rest) = break ( == ':' ) allItems
        rest = strip_leading_char ':' raw_rest
      in
        parse_per_file_columns (next:acc) rest


{-|
  parse list of ints separated by , 
-}
parse_column :: ColumnSpec -> String -> ColumnSpec
parse_column acc []  = acc
parse_column acc fileItems =

   let
     (next,raw_rest) = break ( == ',' ) fileItems
     rest = strip_leading_char ',' raw_rest
   in
     parse_column (read next:acc) rest


{-|
  pad (or trim) [ColumnSpec] to make sure we have the
  same number of elements than we have files 
-}
pad_column_list :: [ColumnSpec] -> Int -> [ColumnSpec]
pad_column_list [] _ = []
pad_column_list xs numFiles 
  | length xs >= numFiles   = take numFiles xs
  | otherwise               = xs ++ pading

    where
      pading :: [ColumnSpec]
      pading  = replicate (numFiles - length xs) $ last xs


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
  putStrLn "pasty 0.1   (C) 2008 Markus Dittrich"
  putStrLn "Usage: pasty <options> file1 file2\n"
  putStrLn "Options:"
  putStrLn "\t-c, --colspecs   columnspecs for parsing"
  putStrLn "\t-u, --outsep     string separating output columns"
  putStrLn "\t                 (default: space)"
  putStrLn "\t-i, --insep      char used for parsing input" 
  putStrLn "\t                 columns greedily (default: space)"
  putStrLn "\t-p, --pastespecs columnspecs for pasting"
  putStrLn ""

