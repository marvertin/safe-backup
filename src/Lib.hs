module Lib
    (
      yabaSuffix,
      HasFileName(fileNamex),
    ) where

import           Data.Function
import           Data.List             (sortBy)
import           System.Directory.Tree (DirTree (Dir, Failed, File), FileName)
import           Text.Printf           (printf)



yabaSuffix = ".yaba"

{- | Dump to lines for debug purpures.
-}
class Dumpable a where
  toDump :: a -> [String]

  toDumpS :: a -> String
  toDumpS = unlines . toDump

  dump :: a -> IO ()
  dump = putStrLn . toDumpS

class  HasFileName a  where
  fileNamex :: a -> FileName


instance HasFileName (DirTree a) where
  fileNamex (Dir name _)    = name
  fileNamex (File name _)   = name
  fileNamex (Failed name _) = name
