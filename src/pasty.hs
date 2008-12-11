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
  this file contains the main driver routines for pasty 
-}


-- imports
import qualified Data.ByteString as B
import System.Environment

-- local imports
import CommandLineParser
import Parser
import PastyData


{-|
  main driver
-}
main :: IO ()
main = 
 
  do
    commandLine <- getArgs
    let parsedCommands = parse_args commandLine
    case parsedCommands of
      Nothing -> print_usage 
      Just (specs,files) -> do
          contents <- read_files [] files
          let columns = extract_columns specs contents
          mapM_ B.putStrLn $ paste (outputSep specs) columns
