module Baup.IO.FileNamesC (
  yabaSuffix,
  configFileName,
  sliceIndexName,
  sliceLogicalTree_suffix,
  modificationTimesFileName,
  indexSubdir,
  dataSubdir,
  logSubdir,
  sliceLogName,
  yabaLogName,
  indexVersion,

)
where

import           System.Directory.Tree (FileName)
--import           System.FilePath (FileName)

yabaSuffix = ".yaba"

configFileName = "yaba-config.yaml" :: FileName

sliceIndexName = indexVersion ++ "_sliceIndex.yaml"
sliceLogicalTree_suffix = "_logical-tree.yaml"

sliceLogName = "slice-backup.log"
yabaLogName = "yaba.log"
indexSubdir = "index"
dataSubdir = "data"
logSubdir = "log"
modificationTimesFileName = "modification-times.yaml"

indexVersion = "1"
