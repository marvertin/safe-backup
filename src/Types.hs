{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}

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

data Ree = Ree { rphysPath :: FilePath, rcount :: Int, rsize :: FileSize, rtime :: UTCTime, rhash :: BS.ByteString }
  deriving (Eq, Show, Read, ToJSON, FromJSON, Generic)

type Hash = BS.ByteString

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


instance ToJSON Hash where
  toJSON hash = toJSON (show hash)

instance FromJSON Hash where
  parseJSON = withText "chuj2" (return . parseee)
    where
      parseee :: Text -> Hash
      parseee x = BS.empty
{-
instance ToJSON Ree where
  -- toJSON (Finfo x y) = object ["x" .= x, "y" .= y]
  toJSON Ree{..} = let val = show rsize ++ " " ++ toHexStr rhash
     in toJSON val

-}
