
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
    (
      readForestDef,
    ) where

import           Control.Exception
import           Data.Bifunctor
import qualified Data.Map          as M
import           Data.Yaml
import           GHC.Generics
import           System.FilePath
import           Types

type ForestDef = [(String, String)]

forestDefName :: FileName
-- forestDefName = "yaba-forest-definition.properties"
forestDefName = "yaba-config.yaml"

newtype Config = Config { forest :: M.Map String String }  deriving (Show, Generic, FromJSON )

readForestDef :: FilePath -> IO (Either ErrMsg ForestDef)
readForestDef backupDir =
 let forestFile = backupDir </> forestDefName
     prependFileName = (("ERROR reading configuration: \"" ++ forestFile ++ "\"\n") ++)
 in  bimap (prependFileName . displayException)
        (M.toList . forest) <$> decodeFileEither forestFile
