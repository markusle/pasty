{-----------------------------------------------------------------
 
  (c) 2008-2013 Markus Dittrich 
 
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
                         , ComLToks(..)
                         ) where


-- imports 
import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as BC
import Control.Monad.State
import Data.List
import Data.Word
import Text.Printf

-- local imports
import ByteStringHelper
import PastyData


{-|
  data structure for describing how command line options are
  to be parsed
-}
data ComLParseTok a = ComLParseTok {
    shortOp    :: String
  , longOp     :: String
  , proc       :: (String -> a)
  , defaultVal :: a
  , info       :: String
}


{-|
  instructions on how to parse the parsing specs [sic]
-}
parseSpecTok :: ComLParseTok [ColumnSpec]
parseSpecTok = ComLParseTok {
    shortOp    = "-r"
  , longOp     = "--parsespecs"
  , proc       = process_parse_specs
  , defaultVal = []
  , info       = "columnspecs for parsing (a1,..:b1,...)"
} 


{-|
  instructions on how to parse for pasting specs
-}
pasteSpecTok :: ComLParseTok ColumnSpec
pasteSpecTok = ComLParseTok {
    shortOp    = "-w"
  , longOp     = "--pastespecs"
  , proc       = process_paste_specs
  , defaultVal = []
  , info       = "columnspecs for pasting (c1,c2,...)"
} 


{-|
  instructions on how to parse for the input separator
-}
insepSpecTok :: ComLParseTok Word8
insepSpecTok = ComLParseTok {
    shortOp    = "-i"
  , longOp     = "--insep"
  , proc       = extract_word8
  , defaultVal = spaceW
  , info       = "char separator for input columns"
} 


{-|
  instructions on how to parse for the input separator
-}
outsepSpecTok :: ComLParseTok B.ByteString
outsepSpecTok = ComLParseTok {
    shortOp    = "-o"
  , longOp     = "--outsep"
  , proc       = BC.pack
  , defaultVal = space
  , info       = "string separator for output columns"
} 


{-|
  definitions for State Monad used for parsing the command
  line
-}
data ComLToks = ComLToks { options:: [String], status :: Bool } 
  deriving(Show)
type ParseState a = State ComLToks a



{-|
  main command line parser
-}
parse_args :: ParseState (OutputSpec,[String])
parse_args = 
  
  parse_option parseSpecTok  >>= \parseS ->
  parse_option pasteSpecTok  >>= \pasteS -> 
  parse_option outsepSpecTok >>= \outsep ->
  parse_option insepSpecTok  >>= \insep  ->
  extract_files              >>= \files  ->
        
  return 
    (OutputSpec { parseSpec = pad_column_list parseS $ length files
                , pasteSpec = pasteS
                , inputSep  = insep
                , outputSep = outsep }
    ,files)


{-|
  the list of files should now be the remainder of
  the command line string after all command line 
  options have been parsed for; we'll also check
  in a very crude way if there are any stray
  command line options left
-}
extract_files :: ParseState [String]
extract_files = get >>= \aState  ->
                 let files = options aState in
                 if check_for_invalid_opts files then
                   let newState = aState { status = False } in
                   put newState >>
                   return []
                 else
                   return files


{-|
  check final list of files for any potentially invalid
  command line options
-}
check_for_invalid_opts :: [String] -> Bool
check_for_invalid_opts [] = False
check_for_invalid_opts (x:xs)
  | head x == '-'         = True
  | otherwise             = check_for_invalid_opts xs



{-|
  parser for an individual command line option
-}
parse_option :: ComLParseTok a -> ParseState a
parse_option parseTokens =

  get >>= \aState ->
  let 
    (e,aState') = parse_option' parseTokens aState
  in
    put aState' >>
    return e

  where
    parse_option' :: ComLParseTok a -> ComLToks -> (a,ComLToks)
    parse_option' (ComLParseTok { proc = f 
                                , shortOp = sOp
                                , longOp = lOp
                                , defaultVal = def })
                   coms@(ComLToks { options = xs })   =

      -- see if we have the short or long option
      case findIndex (==sOp) xs `mplus` findIndex (==lOp) xs of
        Nothing -> (def,coms)
        Just i  ->
          -- see if we have a data element (it should not
          -- begin with a '-', otherwise we set the status
          -- to False
          if ( length xs < i+2 || head value == '-' ) then
            (def, coms { status = False })
          else
            (processedValue,coms { options = remainder })

          where
            option         = xs!!i
            value          = xs!!(i+1)
            processedValue = f value
            remainder      = delete option $ delete value xs



{-|
  parses the user specified list providing the order in with
  the parsed columns are to be punched
-}
process_paste_specs :: String -> ColumnSpec
process_paste_specs = parse_column []



{-|
  parses the user specified list of columns to be extracted for
  each file
-}
process_parse_specs :: String -> [ColumnSpec]
process_parse_specs colSpec = 
  
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
  parse list of ints separated by ',' 
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
  extract a Word8 from a String passed on the command line
  if the string has more than a single char we simple pick
  the first one for now; if the string is empty we take the
  default
-}
extract_word8 :: String -> Word8
extract_word8 item
  | length item == 0    = spaceW
  | otherwise           = char_to_Word8 $ head item


{-|
  print usage information for pasty
-}
print_usage :: IO ()
print_usage = 
  do
    putStrLn "pasty 0.1 (C) 2008-2013 Markus Dittrich"
    putStrLn "Usage: pasty <options> file1 file2\n"
    putStrLn "Options:"
    print_info parseSpecTok
    print_info pasteSpecTok
    print_info insepSpecTok
    print_info outsepSpecTok

    where
      print_info :: ComLParseTok a -> IO ()
      print_info tok = 
        do
          printf "\t%s, %-13s        %s\n" (shortOp tok) (longOp tok)
            (info tok)
