module Types (
  FileSize,
  Hash,
  JabaContent,
  FileName,
  FilePath,
  yabaSuffix
) where

import qualified Data.ByteString       as Strict
import           System.Directory.Tree (DirTree (..), FileName)
import           System.FilePath

type FileSize = Integer
type JabaContent = String

type Hash = Strict.ByteString

yabaSuffix = ".yaba"
