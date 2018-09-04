
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Config
    (
      readConfig,
      pickForestDef,
      pickIgnorenceDef,
      ForestDef,
    ) where

import           Control.Exception
import           Data.Bifunctor
import qualified Data.Map          as M
import           Data.Maybe
import           Data.Yaml
import           GHC.Generics
import           Ignorances
import           System.FilePath
import           Types

type ForestDef = [(String, FilePath, IgnoranceDef)]


data CfgTree = CfgTree { path :: FilePath, filter :: Maybe [String]} deriving (Show, Generic, FromJSON )
newtype Config = Config { forest :: M.Map String CfgTree }  deriving (Show, Generic, FromJSON )

pickForestDef :: Config -> ForestDef
pickForestDef = map (\(treename, CfgTree{..}) -> (treename, path, fromMaybe [] filter) )
  . M.toList . forest

pickIgnorenceDef :: Config -> IgnoranceDef
pickIgnorenceDef _ =  []


readConfig :: FilePath -> IO (Either ErrMsg Config)
-- readConfig backupDir = decodeFileEither $ backupDir </> configFileName
readConfig backupDir =
 let configFile = backupDir </> configFileName
     prependFileName = (("ERROR reading configuration: \"" ++ configFile ++ "\"\n") ++)
 in  bimap (prependFileName . displayException) id <$> decodeFileEither configFile
