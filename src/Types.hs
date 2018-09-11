{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types (
  RevPath,
  Ree(..),
  FileSize,
  Hash,
  FileName,
  FilePath,
  ErrMsg,
  ErrList(..),
  yabaSuffix,
  configFileName,
  slicePhysicalTree_suffix,
  sliceLogicalTree_suffix,
  indexSubdir,
  dataSubdir,
  logSubdir,
  sliceLogName,
  yabaLogName,
  indexVersion,

  UTCTime
) where

import qualified Data.ByteString       as BS
import           Data.Text
import           Data.Time.Clock
import           Data.Yaml
import           GHC.Generics
import           System.Directory.Tree (DirTree (..), FileName)
import           System.FilePath

type FileSize = Integer

type RevPath = [String] -- it is reverse list of path items: ["myfile.txt", "myaccount", "home", "opt"]

--deriving instance ToJSON BS.ByteString
--deriving instance FromJSON BS.ByteString

data Ree = Ree { rcount :: Int, rsize :: FileSize, rtime :: UTCTime, rhash :: BS.ByteString }
  deriving (Eq, Show, Read, ToJSON, FromJSON, Generic)

type Hash = BS.ByteString

type ErrMsg = String

newtype ErrList = ErrList { getErrList :: [String] } deriving (Show)

yabaSuffix = ".yaba"

configFileName = "yaba-config.yaml" :: FileName

slicePhysicalTree_suffix = "_physical-tree.yaml"
sliceLogicalTree_suffix = "_logical-tree.yaml"

sliceLogName = "slice-backup.log"
yabaLogName = "yaba.log"
indexSubdir = "index"
dataSubdir = "data"
logSubdir = "log"

indexVersion = "1"

instance ToJSON Hash where
  toJSON hash = toJSON (show hash)

instance FromJSON Hash where
  parseJSON = withText "chuj2" (return . parseee)
    where
      parseee :: Text -> Hash
      parseee x = (read (unpack x) :: Hash)

      {-

instance ToJSON Ree where
  -- toJSON (Finfo x y) = object ["x" .= x, "y" .= y]
  toJSON Ree{..} = let val = show rsize ++ " " ++ toHexStr rhash
     in toJSON val

-}
