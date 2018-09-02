{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types (
  Ree(..),
  FileSize,
  Hash,
  FileName,
  FilePath,
  ErrMsg,
  yabaSuffix,
  yabaSliceSuffix,
  configFileName,
  slicePhysicalTree_suffix,
  sliceLogicalTree_suffix,
  sliceSourceTree_suffix,
  indexSubdir,
  dataSubdir,
  logSubdir,
) where

import qualified Data.ByteString       as Strict
import           Data.Yaml
import           System.Directory.Tree (DirTree (..), FileName)
import           System.FilePath

type FileSize = Integer

data Ree = Ree { rphysPath :: FilePath, rcount :: Int, rsize :: FileSize, rhash :: Hash }
  deriving (Eq, Show, Read)

type Hash = Strict.ByteString

type ErrMsg = String

yabaSuffix = ".yaba"

yabaSliceSuffix = ".yaba-slice"

configFileName = "yaba-config.yaml" :: FileName

slicePhysicalTree_suffix = "_physical-tree.yaml"
sliceLogicalTree_suffix = "_logical-tree.yaml"
sliceSourceTree_suffix = "_source-tree.yaml"

indexSubdir = "index"
dataSubdir = "data"
logSubdir = "log"
