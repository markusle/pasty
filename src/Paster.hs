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
  this file contains the main pasting routines
-}

module Paster ( paste ) where



-- imports
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
--import Data.Word

-- local imports
import ByteStringHelper
import PastyData


{-| 
  type declarations purely for notational convenience 
-}
type Column       = [B.ByteString]


{-|
  concatenate a collection of columns row wise and return 
  the resulting concatenated column
-}
paste :: OutputSpec -> [Column] -> Column
paste _ []           = []
paste specs cols = paste_h [] $ pick_columns colOrder cols

  where
    separator = outputSep specs

    -- retrieve the requested order for pasting columns; ignore
    -- all column indices that are larger than the number of 
    -- columns we have available
    colOrder  = filter (< length cols) $ pasteSpec specs

    -- paste helper
    paste_h :: Column -> [Column] -> Column
    paste_h acc []       = acc
    paste_h acc (x:xs)   = 
      paste_h (walk_column [] separator acc x) xs

{-|
  walk down a column and append content to accumulator row-wise 
  FIXME: The call to revers after each column walk sucks and is
         inefficient; is there a way to get rid of it??      
-}
walk_column :: Column -> B.ByteString -> Column -> Column -> Column
walk_column acc _ [] []           = reverse acc
walk_column acc sep [] (r:rs)     = walk_column (r:acc) sep [] rs
walk_column acc sep (l:ls) []     = walk_column (l:acc) sep ls []
walk_column acc sep (l:ls) (r:rs) = walk_column (cat_columns sep l 
                                      r:acc) sep ls rs


{-|
  pick columns according to requested specs; if none is
  requested we simply use the input order
-}
pick_columns :: ColumnSpec -> [Column] -> [Column]
pick_columns [] x         = x
pick_columns _ []         = []
pick_columns colSpec columns = pick_columns_h [] colSpec columns

  where
    -- helper function dealing with picking the request columns
    -- for pasting to output
    pick_columns_h :: [Column] -> ColumnSpec -> [Column] -> [Column]
    pick_columns_h acc [] _        = acc
    pick_columns_h _   _ []        = []
    pick_columns_h acc (x:xs) cols = 
      pick_columns_h ( cols !! x:acc) xs cols
