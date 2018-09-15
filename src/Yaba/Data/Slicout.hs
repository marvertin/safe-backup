{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Yaba.Data.Slicout (
  Slicout,
  AnchoredSlicout,
  Cmd(..),
  Paths(..),
  Info(..),
  sizeToBackup,
  countsToBackup,
  modificationTimes,
  countCounters,
  kindOfChange
) where

import           Data.Counter
import           Data.List
import qualified Data.Map              as M

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


sizeToBackup :: Slicout -> Integer
sizeToBackup bt = sum $ fmap mapa bt
   where mapa (Insert sz _) = sz
         mapa _             = 0

countsToBackup :: Slicout -> Integer
countsToBackup bt = sum $ fmap mapa bt
  where mapa (Insert _ _) = 1
        mapa _            = 0

modificationTimes :: Slicout ->  M.Map FilePath UTCTime
modificationTimes bt = M.fromList . sort $ foldMap extractTime (zipPaths' bt)
  where
     extractTime (fp, (Insert _ time)) = [(fp, time)]
     extractTime _                     = []


countCounters :: Slicout -> Counter String Int
countCounters = count . (foldMap (return . kindOfChange))


kindOfChange :: Cmd -> String
kindOfChange (Delete (Info _ (Paths {pathsNew=[]}) _ ))=    "~DELETE~"
kindOfChange (Delete _)=                                    "~MOVE-AWAY~"
kindOfChange (Link _ (Info _ (Paths {pathsHistory=[]}) _ ))="~N-LINK~"
kindOfChange (Link _ _)                                     = "~LINK~"
kindOfChange (Insert{})                                     = "~INSERT~"


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
