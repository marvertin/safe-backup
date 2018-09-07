{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module SliceNameStrategy (
  SliceNameStrategy,
  defaultSliceNameStrategy,

  listSlices,
  nextSliceName
) where

import           Data.List
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Yaml
import           GHC.Generics
import           System.Directory
import           System.FilePath


defaultSliceNameStrategy = UtcSeconds

data SliceNameStrategy = UtcSeconds | Numbers4
   deriving (Show, Generic, FromJSON, ToJSON)


listSlices :: FilePath -> SliceNameStrategy -> IO [FilePath]
listSlices sliceRoot _ = sort <$> listDirectory sliceRoot


nextSliceName :: FilePath -> SliceNameStrategy -> IO FilePath
nextSliceName sliceRoot strategy = do
  now <- getCurrentTime
  return $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H-%M-%SZ")) now
