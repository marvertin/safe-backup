module Lib
    (
      HasFileName(..),
    ) where

import           Data.Function
import           Data.List             (sortBy)
import           System.Directory.Tree (DirTree (Dir, Failed, File), FileName)
import           Text.Printf           (printf)


class  HasFileName a  where
  fileNamex :: a -> FileName


instance HasFileName (DirTree a) where
  fileNamex (Dir name _)    = name
  fileNamex (File name _)   = name
  fileNamex (Failed name _) = name
