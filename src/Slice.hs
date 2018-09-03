{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Slice
    (
    formatMetaFile,
    parseMetaFile,
    SliceFile(..),
    SliceTree(..),
    SliceCmd(..),
    AnchoredSliceTree
    ) where

import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Dump
import           GHC.IO.Encoding
import           Lib
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO             (hFlush, stdout)
import           Text.Printf           (printf)
import           TurboWare
import           Types

import qualified Crypto.Hash.SHA1      as Cr
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.ByteString.UTF8  as BSU

import qualified Data.Map              as M
import           Data.Yaml
import           GHC.Generics



type SliceTree = DirTree SliceFile
type AnchoredSliceTree = AnchoredDirTree SliceFile


data SliceFile = RegularFile Ree
              |  MetaFile SliceCmd  -- file content
              deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data SliceCmd = Delete | LogicalLink FilePath | PhysicalLink FilePath
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)


parseMetaFile :: String -> SliceCmd
parseMetaFile fileContent = (parse . lines) fileContent
  where
    parse :: [String] -> SliceCmd
    parse []                   = error "yaba file is empty"
    parse ("#yaba1" : line : _) = read line
    parse _ = error $ "Bad version of Yaba file, probably old version of yaba tool: " ++ show fileContent

formatMetaFile :: SliceCmd -> String
formatMetaFile x = unlines ["#yaba1", show x, "------------------------"]

isSliceRegularFile :: SliceTree -> Bool
isSliceRegularFile (File _ (RegularFile _)) = True
isSliceRegularFile _                        = False

isSliceMetaFile :: SliceTree -> Bool
isSliceMetaFile (File _ (MetaFile _)) = True
isSliceMetaFile _                     = False


printSliceFile :: SliceFile ->  Maybe String
printSliceFile (RegularFile Ree{..}) = Just$ "  #" ++ show rsize ++ " " ++ toHexStr rhash ++ " \"" ++ rphysPath ++ "\""
printSliceFile (MetaFile sliceCmd) = Just$  show sliceCmd

printSliceFile2 :: SliceFile -> Maybe String
printSliceFile2 (RegularFile Ree{rphysPath}) = Just rphysPath
printSliceFile2 (MetaFile _)                 = Nothing

--------------------------------------------------------

instance Dumpable SliceTree where
  toDump = dirTreeToStringList printSliceFile

{-
deriving instance Generic SliceTree
deriving instance ToJSON SliceTree
deriving instance FromJSON SliceTree

deriving instance Generic IOException
deriving instance ToJSON IOException
deriving instance FromJSON IOException

-}


  --------------------------------------------------------
  --
  -- Instances for YAML

instance ToJSON SliceTree where
     toJSON (File _ x)   = toJSON x
     toJSON (Dir _ list) = toJSON $ M.fromList (tupl <$> list)
       where
        tupl q@(File name _) = (name, q)
        tupl q@(Dir name _)  = (name, q)
{-
instance ToJSON SliceFile where
  toJSON (RegularFile Ree{..}) = toJSON $ show rsize ++ " " ++ toHexStr rhash
  toJSON (MetaFile x)          = toJSON x

-}
