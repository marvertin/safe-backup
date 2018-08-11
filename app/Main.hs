module Main where

import           Data.Maybe
import           GHC.IO.Encoding
--import           Filesystem.Path
import           Debug
import           Lib
import           LogicalDirTree
import           System.Directory.Tree
import           System.FilePath.Find
import           TreeComparator
import           TurboWare
import           YabaDirTree           hiding (RegularFile)



main :: IO ()
main = do
  setLocaleEncoding utf8
  getLocaleEncoding >>= print
  -- putStrLn "→"
