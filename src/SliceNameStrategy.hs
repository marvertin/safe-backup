{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module SliceNameStrategy (
  SliceNameStrategy
) where

import           Data.Yaml
import           GHC.Generics


data SliceNameStrategy = UtcSeconds | Numbers4
   deriving (Show, Generic, FromJSON, ToJSON)
