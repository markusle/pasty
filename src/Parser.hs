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
  this file contains the main parsing routines
-}

module Parser ( ColumnSpec
              , extract_columns
              , paste
              , read_files
              ) where



-- imports
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.Word

-- local imports
import ByteStringHelper
import IO
import PastyData


{-| 
  type declarations purely for notational convenience 
-}
type FileContents = B.ByteString
type Column       = [B.ByteString]


{-|
  concatenate a collection of columns row wise and return 
  the resulting concatenated column
-}
paste :: B.ByteString -> [Column] -> Column
paste _ []           = []
paste separator cols = paste_column [] separator cols 

  where
    -- past columns
    paste_column :: Column -> B.ByteString -> [Column] -> Column
    paste_column acc _ []       = acc
    paste_column acc sep (x:xs) = 
      paste_column (walk_column [] acc sep x) sep xs

    -- walk down a column and append content to accumulator row-wise 
    -- FIXME: The call to revers after each column walk sucks and is
    --        inefficient; is there a way to get rid of it??       -}
    walk_column :: Column -> Column -> B.ByteString -> Column -> Column
    walk_column acc [] _ [] = reverse acc
    walk_column acc [] s (r:rs) = walk_column (r:acc) [] s rs
    walk_column acc (l:ls) s [] = walk_column (l:acc) ls s []
    walk_column acc (l:ls) s (r:rs) =
      walk_column (cat_columns s l r:acc) ls s rs



{-|
  extract the columns corresponding to the ColSpecs for all
  files
-}
extract_columns :: OutputSpec -> [FileContents] -> [Column]
extract_columns _ []            = []
extract_columns spec xs 
  | colList == []               = split_into_lines xs
  | otherwise                   = grab_all_columns [] colList $ 
                                    split_into_lines xs 
  where
  -- extract relevant command lin options
  itemSep = inputSep spec
  colList = columnSpec spec

  -- grab all selected columns from the input files
  grab_all_columns :: [Column] -> [ColumnSpec] -> [Column] -> [Column]
  grab_all_columns acc [] _  = acc
  grab_all_columns acc _ []  = acc
  grab_all_columns acc (c:cs) (y:ys) = 
    grab_all_columns (acc ++ grab_file_columns itemSep c y) cs ys



{-|
  extract the specified columns from the content of a particular
  file
-}
grab_file_columns :: Word8 -> ColumnSpec -> Column -> [Column]
grab_file_columns itemSep colSpec col = scan_row [] colSpecGood 
                                          colItems

  where 
    colItems = map (\x -> split_into_items itemSep x) col

    -- filter all colSpecs that index outside the line length
    minLength = minimum $ map length colItems
    colSpecGood = filter ( < minLength ) $ reverse colSpec



{-|
  scan a single row that has been itemized into whitespace
  separated items and extract the requested column elements 
-}
scan_row :: [Column] -> ColumnSpec -> [Column] -> [Column]
scan_row acc [] _     = acc
scan_row acc _  []    = acc
scan_row acc (c:cs) items = 
  scan_row (map ( !! c) items:acc) cs items


