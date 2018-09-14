{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Yaba.Data.Slicout (
  Slicout,
  AnchoredSlicout,
  Cmd(..),
  Paths(..),
  Info(..)
) where

import           Dump
import           Lib
import           System.Directory.Tree
import           TurboWare
import           Types
import           Yaba.Data.Lodree




data Info = Info Hash Paths Lodree  deriving (Show) -- gives information only to peaple, not processed by machine
data Cmd = Insert Integer UTCTime | Delete Info | Link FilePath Info  deriving (Show)
data Paths = Paths { pathsNew :: [FilePath], pathsLast:: [FilePath], pathsHistory :: [FilePath] }  deriving (Show)

type Slicout = DirTree Cmd
type AnchoredSlicout = AnchoredDirTree Cmd




instance Dumpable Cmd where
    toDump x = [ "**" ++ show x ]
    toDumpS = tostra
      where
          tostra (Insert size time) = "Insert " ++ (showSz size) ++ " " ++ (show time)
          tostra x             = show x


instance Dumpable Slicout where
  toDump = dirTreeToStringList printCmd

printCmd :: Cmd ->  Maybe String
printCmd (Insert {}) = Just "<insert>"
printCmd (Delete {}) = Just "<delete>"
printCmd (Link fp _) = Just $ "<link \"" ++ fp ++  "\" >"
