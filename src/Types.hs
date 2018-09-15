{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types (
  RevPath,
  FileSize,
  Hash,
  FileName,
  FilePath,
  ErrMsg,
  ErrList(..),


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

type Hash = BS.ByteString

type ErrMsg = String

newtype ErrList = ErrList { getErrList :: [String] } deriving (Show)


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
