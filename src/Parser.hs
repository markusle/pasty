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
              ) where



-- imports
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import Data.Word

-- local imports
import ByteStringHelper
import PastyData


{-| 
  type declarations purely for notational convenience 
-}
type FileContents = B.ByteString
type Column       = [B.ByteString]


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
  colList = parseSpec spec

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


