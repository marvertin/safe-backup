module Types (
  FileSize,
  Hash,
  JabaContent(..),
  FileName,
  FilePath,
  yabaSuffix
) where

import qualified Data.ByteString       as Strict
import           System.Directory.Tree (DirTree (..), FileName)
import           System.FilePath

type FileSize = Integer
newtype JabaContent = JabaContent { unJabaContent :: String } deriving (Show)

type Hash = Strict.ByteString

yabaSuffix = ".yaba"
