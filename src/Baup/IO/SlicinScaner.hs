{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Baup.IO.SlicinScaner
    (
    readSlice,
    ) where

import           Control.Exception
import           Control.Monad
import qualified Crypto.Hash.SHA1      as Cr
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.ByteString.UTF8  as BSU
import           Data.List
import qualified Data.Map              as M
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Data.Yaml
import           GHC.Generics
import           GHC.IO.Encoding
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO             (hFlush, stdout)
import           Text.Printf           (printf)

import           Baup.Data.Ree
import           Baup.Data.Slicin
import           Baup.IO.FileNamesC
import           Util.DirScan
import           Util.Dump
import           Util.Lib
import           Util.TurboWare
import           Util.Types




readSlice :: EventHandler Slicin ErrList -> FilePath -> IO (Slicin, ErrList)
readSlice eventHandler rootDir =
    scanDirectory mkDir filterFilesInRoot readSFile eventHandler
        (replaceVerticalToSlashes rootDir) -- >>= ((takeDirectory rootDir ):/)
  where
    -- rootDir1 = (takeFileName . takeDirectory) rootDir ++ "|" ++ takeFileName rootDir -- it os not filename but root directory
    rootDir1 = takeFileName rootDir -- it is not filename but root directory
    readSFile :: RevPath -> IO Slicin
    readSFile rp = File (head rp) <$> loadSliceFile rootDir rp

    mkDir rp list = Dir (safeHead rootDir1 rp) (filter (not . isEmptyDir) . fmap snd $ list)

    filterFilesInRoot [fordName] = takeExtension fordName /= ".yaml"
    filterFilesInRoot _          = True



loadSliceFile :: FilePath -> RevPath -> IO SliceFile
loadSliceFile rootPath rp = do
  let path = replaceBacklashesToSlashes (pth rp)
  let realPath = replaceVerticalToSlashes (rootPath </> path)
  size <- getFileSize realPath
  time <- getModificationTime realPath
  hash <- computeFileHash realPath
  let originalPath = "/" ++ takeFileName rootPath ++ "/" ++ path
  if not $ metaSuffix `isSuffixOf` path then return (RegularFile  (Ree 1 size time hash) originalPath )
          else (MetaFile . parseMetaFile . T.unpack) <$> TIO.readFile realPath

isEmptyDir :: Slicin -> Bool
isEmptyDir (Dir _ []) = True
isEmptyDir _          = False
