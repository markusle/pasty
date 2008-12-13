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
  ByteStringHelper provides utility functions to deal with
  ByteStrings
-}
module ByteStringHelper ( cat_columns
                        , char_to_Word8
                        , newLineW
                        , space
                        , spaceW
                        , split_into_items
                        , split_into_lines
                        ) where



-- imports
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Data.Word
--import Debug.Trace

{-|
  convert char into Word8
-}
char_to_Word8 :: Char -> Word8
char_to_Word8 = fromIntegral . ord


{-|
  a few useful Word8s
-}
newLineW :: Word8
newLineW = char_to_Word8 '\n'

spaceW :: Word8
spaceW = char_to_Word8 ' '



{-|
  a few useful ByteStrings
-}
space :: B.ByteString
space = BC.pack " "


{-|
  if the last element in a ByteString is a newline,
  discard it, otherwise splitting the ByteString by
  newlines will result in an empty last element
-}
remove_last_newline :: B.ByteString -> B.ByteString
remove_last_newline string 
  | B.last string == newLineW  = B.init string 
  | otherwise                  = string



{-|
  split a list of ByteStrings into its constituent lines, 
  i.e., bytes separated by a newline '\n'
-}
split_into_lines :: [B.ByteString] -> [[B.ByteString]]
split_into_lines = map (B.split newLineW . remove_last_newline) 



{-|
  split a single ByteString into its constituent items, i.e., bytes
  separated by any number of spaceChars.
-}
split_into_items :: Word8 -> B.ByteString -> [B.ByteString]
split_into_items inSep = filter ( /= B.empty ) . B.split inSep 



{-|
  concatenate two column entries consisting of ByteStrings
  and an intervening ByteString (unless the "left" item is an empty
  bytestring) to give a new row;
-}
cat_columns :: B.ByteString -> B.ByteString -> B.ByteString 
               -> B.ByteString
cat_columns sep left right 
  | left == B.empty  = right
  | otherwise        = B.append left $ B.append sep right



