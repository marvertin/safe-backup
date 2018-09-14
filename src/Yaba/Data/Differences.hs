{-# LANGUAGE InstanceSigs #-}

module Yaba.Data.Differences (
  DirCompare(..)
) where

import           TurboWare
import           Types
import           Yaba.Data.Lodree

data DirCompare = QDir [(FileName, DirCompare)]
 | QLeft Lodree
 | QRight Lodree
 | QBoth Lodree Lodree

 -------------------------------------------------------------------
 -- The rest of this modul is for DEBUGING purpose only - it is dump
 --
instance Dumpable DirCompare where
 toDump :: DirCompare -> [String]

 toDump (QLeft (LFile _ _)) = ["- . "]
 toDump (QLeft (LDir _ _)) = ["- / "]
 toDump (QRight (LFile _ _)) = ["+ . "]
 toDump (QRight (LDir _ _)) = ["+ / "]
 toDump (QBoth LFile {} LFile {}) = ["~ .."]
 toDump (QBoth LFile {} LDir {}) = ["~ ./"]
 toDump (QBoth LDir {} LFile {}) = ["~ /."]
 toDump (QBoth LDir {} LDir {}) = ["~ //"]

 toDump (QDir items) = ("       " ++) <$> (items >>= todump)
    where
       todump :: (FileName, DirCompare) -> [String]
       todump (filename, dc@(QDir dir)) = ("/    " ++ filename) : toDump dc
       todump (filename, dc) = appendToFirst (" " ++ filename) (toDump dc)
