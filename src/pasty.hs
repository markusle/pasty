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
  this file contains the main driver routines for pasty 
-}


-- imports
import qualified Data.ByteString.Char8 as B
import Control.Monad.State
import System.Environment

-- local imports
import CommandLineParser
import IO
import Parser
import Paster


{-|
  main driver
-}
main :: IO ()
main = getArgs >>= \x ->
       let 
         -- define the intial state for the State Monad used
         -- for parsing the command line
         initialState = ComLToks { options = x, status = True }
         ((specs,files), aState) = runState parse_args $ initialState
       in
         -- if something bad happened or no files where supplied
         -- we bail out
         if status aState == False || null files 
           then print_usage
           else 
             read_files [] files >>= \y ->
             let columns = extract_columns specs y in
               mapM_ B.putStrLn $ paste specs columns
