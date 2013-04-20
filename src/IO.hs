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
  this file contains the IO related functions
-}

module IO ( read_files
          , try_read_file 
          ) where



-- imports
import qualified Data.ByteString as B
import Control.Exception (try)
import System.IO.Error


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
