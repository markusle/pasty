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
module ByteStringHelper ( remove_last_newline,
                          equilize,
                          newLineW
                        ) where

-- imports
import Char
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Word


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


{-|
  a few useful ByteStrings
-}
newLine :: B.ByteString
newLine = BC.pack "\n"


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
  equilize acts on a list of ByteStrings: It finds the one(s)
  with the most newlines and pads all the other ones with
  newlines so that in the end all have the same length
-}
equilize :: [B.ByteString] -> [B.ByteString]
equilize []    = []
equilize items = pad_items [] maxLength items

  where
    maxLength = find_longest items
    
    find_longest :: [B.ByteString] -> Int
    find_longest = maximum . map count_newlines

    pad_items :: [B.ByteString] -> Int -> [B.ByteString] 
              -> [B.ByteString]
    pad_items acc _ []     = acc
    pad_items _   0 xs     = xs
    pad_items acc p (x:xs) = 
      pad_items (B.append x (add_newlines p x):acc) p xs


{-|
  return a number n of newlines such that the current number
  in the ByteString plus n equals the target
-}
add_newlines :: Int -> B.ByteString -> B.ByteString
add_newlines 0 _       = B.empty
add_newlines target x  = 
  let
    currentNum = count_newlines x
    difference = target - currentNum
  in
    newlines difference


{-|
  returns a ByteString constisting of n newlines
-}
newlines :: Int -> B.ByteString
newlines num = cat_newlines [] num

  where
    cat_newlines :: [B.ByteString] -> Int -> B.ByteString
    cat_newlines acc 0 = B.concat acc
    cat_newlines acc x = cat_newlines (newLine:acc) (x-1)


{-|
  counts the number of newlines in a ByteString
-}
count_newlines :: B.ByteString -> Int
count_newlines = length . B.findIndices ( == newLineW )
