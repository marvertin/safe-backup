{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module SourceTree (
readSourceTree,
) where

import qualified Crypto.Hash.SHA1      as Cr

import qualified Data.ByteString       as Strict
import qualified Data.ByteString.UTF8  as BSU
import           Data.Function
import           Data.List
import           Data.Maybe
import           Lib
import           Lodree
import           SliceMerger
import           System.Directory.Tree (AnchoredDirTree (..),
                                        DirTree (Dir, File), FileName)
import           System.FilePath
import           TurboWare
import           Types
import           YabaDirTree
import           YabaFileContent

readSourceTree :: FileName -> IO Lodree
readSourceTree dirName = do
    (base :/ d) <- readYabaDir dirName
    return$ currentLodree $ mergeToLodree emptyLodree d
