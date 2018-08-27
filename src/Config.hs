
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
    (
      readConfig,
      pickForestDef,
    ) where

import           Control.Exception
import           Data.Bifunctor
import qualified Data.Map          as M
import           Data.Yaml
import           GHC.Generics
import           System.FilePath
import           Types

type ForestDef = [(String, String)]


data CfgTree = CfgTree { path :: FilePath, ignore :: Maybe [FilePath]} deriving (Show, Generic, FromJSON )
newtype Config = Config { forest :: M.Map String CfgTree }  deriving (Show, Generic, FromJSON )

pickForestDef :: Config -> ForestDef
pickForestDef = map (fmap path) . M.toList . forest


readConfig :: FilePath -> IO (Either ErrMsg Config)
-- readConfig backupDir = decodeFileEither $ backupDir </> configFileName
readConfig backupDir =
 let configFile = backupDir </> configFileName
     prependFileName = (("ERROR reading configuration: \"" ++ configFile ++ "\"\n") ++)
 in  bimap (prependFileName . displayException) id <$> decodeFileEither configFile
