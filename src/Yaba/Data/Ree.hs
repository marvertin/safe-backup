{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}


module Yaba.Data.Ree (
  Ree(..),
  CacheHash,
  loadFileRee,
  showRee,


) where

import qualified Data.ByteString  as BS
import qualified Data.Map         as M
import           Data.Time.Clock
import           Data.Yaml
import           GHC.Generics
import           System.Directory
import           Text.Printf

import           Util.Lib
import           Util.Types



data Ree = Ree { rcount :: Int, rsize :: FileSize, rtime :: UTCTime, rhash :: BS.ByteString }
  deriving (Eq, Show, Read, ToJSON, FromJSON, Generic)

type CacheHash = M.Map RevPath Ree

showRee :: Ree -> String
showRee Ree{..} = printf "%d files, %s" rcount (showSz rsize)


loadFileRee :: FilePath -> IO Ree
loadFileRee f = Ree 1 <$> getFileSize f <*> getModificationTime f <*> computeFileHash f
