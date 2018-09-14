{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module SourceTree (
 readSourceTree,
 CacheHash
) where

import qualified Crypto.Hash.SHA1          as Cr

import           Control.Exception
import qualified Data.ByteString           as Strict
import qualified Data.ByteString.UTF8      as BSU
import           Data.Function
import           Data.List
import qualified Data.Map                  as M
import           Data.Maybe
import           Data.Time.Clock
import           System.Directory
import           System.Directory.Tree     (AnchoredDirTree (..),
                                            DirTree (Dir, File), FileName)
import           System.FilePath
import           System.IO


import           DirScan
import           Ignorances
import           Lib
import           Log
import           SliceScaner
import           TurboWare
import           Types
import           Yaba.Data.Lodree
import           Yaba.Process.SlicinMerger


import qualified Data.ByteString.Lazy      as Lazy

type CacheHash = M.Map RevPath Ree

getEventHandler :: UTCTime -> Log -> (EventEnvelop a ErrList -> IO ErrList, ErrList)
getEventHandler time lo  = (logInScan time lo, ErrList [])

-- EventHandler Lodree b
readSourceTree :: Log -> CacheHash -> IgnoranceDef -> FilePath -> IO (Lodree, ErrList)
readSourceTree lo cacheHash ignorance rootDir = do
     startTime <- getCurrentTime
     scanDirectory (const makeLDir)
                     (makeFilterFce ignorance)
                     readLFile
                     (getEventHandler startTime lo)
                     rootDir
  where
    readLFile :: RevPath -> IO Lodree
    readLFile rp = do
        let path  = rootDir </> pth rp
        size <- getFileSize path
        time <- getModificationTime path
        flip LFile (pth rp) <$>
           case M.lookup rp cacheHash of
                 Nothing -> loadFileRee path
                 Just ree@Ree{..} -> if size == rsize && time == rtime
                                         then return ree
                                         else loadFileRee path
