{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types (
  Ree(..),
  FileSize,
  Hash,
  JabaContent(..),
  FileName,
  FilePath,
  ErrMsg,
  yabaSuffix,
  yabaSliceSuffix,
  configFileName,
  yabaSliceTree,
  yabaSrcTree,
  yabaLodreeTree
) where

import qualified Data.ByteString       as Strict
import           Data.Yaml
import           System.Directory.Tree (DirTree (..), FileName)
import           System.FilePath

type FileSize = Integer
newtype JabaContent = JabaContent { unJabaContent :: String } deriving (Show, ToJSON, FromJSON)

data Ree = Ree { rphysPath :: FilePath, rcount :: Int, rsize :: FileSize, rhash :: Hash }
  deriving (Eq, Show, Read)

type Hash = Strict.ByteString

type ErrMsg = String

yabaSuffix = ".yaba"

yabaSliceSuffix = ".yaba-slice"

configFileName = "yaba-config.yaml" :: FileName

yabaSliceTree = "~yaba-slice-physical-tree.yaml"
yabaSrcTree = "~yaba-src-tree.yaml"
yabaLodreeTree = "~yaba-slice-logical-tree"
