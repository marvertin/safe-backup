{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module SliceNameStrategy (
  SliceNameStrategy,
  defaultSliceNameStrategy
) where

import           Data.Yaml
import           GHC.Generics

defaultSliceNameStrategy = UtcSeconds

data SliceNameStrategy = UtcSeconds | Numbers4
   deriving (Show, Generic, FromJSON, ToJSON)
