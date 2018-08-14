module Main where

import           Data.Maybe
import           GHC.IO.Encoding
--import           Filesystem.Path
import           Backup
import           Debug
import           Lib
import           LogicalDirTree
import           System.Directory.Tree
import           System.FilePath.Find
import           TreeComparator
import           TurboWare
import           YabaDirTree           hiding (RegularFile)

--import           Data.Time.Calendar
import           Data.Time.Clock

main :: IO ()
main = do
  setLocaleEncoding utf8
  getLocaleEncoding >>= print
  now <- getCurrentTime
  print now
  -- putStrLn "→"
