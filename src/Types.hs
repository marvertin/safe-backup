{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types (
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

type Hash = Strict.ByteString

type ErrMsg = String

yabaSuffix = ".yaba"

yabaSliceSuffix = ".yaba-slice"

configFileName = "yaba-config.yaml" :: FileName

yabaSliceTree = "~yaba-slice-physical-tree.yaml"
yabaSrcTree = "~yaba-src-tree.yaml"
yabaLodreeTree = "~yaba-slice-logical-tree"
