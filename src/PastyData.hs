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
  common data structures used by many pasty modules
  command line
-}
module PastyData ( OutputSpec(..)
                 , ColumnSpec
                 ) where

-- imports
import qualified Data.ByteString as B 
import Data.Word


{-|
  notational type definitions
-}
type ColumnSpec  = [Int]



{-|
  data structure for keeping track of the parsed command line info
-}
data OutputSpec = OutputSpec {
    parseSpec :: [ColumnSpec]
  , pasteSpec :: ColumnSpec
  , inputSep  :: Word8
  , outputSep :: B.ByteString
}

