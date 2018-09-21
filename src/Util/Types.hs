{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Util.Types (
  RevPath,
  FileSize,
  FilesCount,
  Hash,
  FileName,
  FilePath,
  ErrMsg,
  ErrList(..),
  SliceName,
  pgmname,
  pgmshortdesc,


  UTCTime
) where

import qualified Data.ByteString       as BS

import           Data.Text
import           Data.Time.Clock
import           Data.Yaml
import           GHC.Generics
import           System.Directory.Tree (DirTree (..), FileName)
import           System.FilePath

-- | size of file
type FileSize = Integer

-- | count of files
type FilesCount = Int

-- | it is reverse list of path items: ["myfile.txt", "myaccount", "home", "opt"]
type RevPath = [String]

type Hash = BS.ByteString

type ErrMsg = String

-- | slcienamei of the form "2008-02|12|156" It contains vertical lines, not slashes.
type SliceName = String

newtype ErrList = ErrList { getErrList :: [String] } deriving (Show)


instance ToJSON Hash where
  toJSON hash = toJSON (show hash)

instance FromJSON Hash where
  parseJSON = withText "chuj2" (return . parseee)
    where
      parseee :: Text -> Hash
      parseee x = (read (unpack x) :: Hash)

pgmname = "yaba"
pgmshortdesc = "yeat another backup"
      {-

instance ToJSON Ree where
  -- toJSON (Finfo x y) = object ["x" .= x, "y" .= y]
  toJSON Ree{..} = let val = show rsize ++ " " ++ toHexStr rhash
     in toJSON val

-}
