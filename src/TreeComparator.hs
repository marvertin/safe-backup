{-# LANGUAGE InstanceSigs #-}

module TreeComparator (
  compareTrees,
  DirCompare(..)
) where


import           Data.Maybe
import           Lib
import           Lodree
import           TurboWare
import           Types

data DirCompare = QDir [(FileName, DirCompare)]
 | QLeft Lodree
 | QRight Lodree
 | QBoth Lodree Lodree

compareTrees :: Lodree -> Lodree -> Maybe DirCompare
compareTrees l r
  | hashLodree l == hashLodree r = Nothing
compareTrees (LDir _ ls) (LDir _ rs) = let
   list :: [(FileName, Maybe (FileName, Lodree), Maybe (FileName, Lodree))]
   list = zipMaybe fst fst ls rs
   compareDirs2 (fileName, l, r) =  fmap ((,) fileName) (compareDirs (snd <$> l) (snd <$> r))
  in Just $ QDir (mapMaybe compareDirs2 list)
  where
     compareDirs :: Maybe Lodree -> Maybe Lodree -> Maybe DirCompare
     compareDirs Nothing Nothing   = error "Imposible has happend! No dir, no file"
     compareDirs (Just l) Nothing  = Just$ QLeft l
     compareDirs Nothing (Just r)  = Just$ QRight r
     compareDirs (Just l) (Just r) = compareTrees l r
compareTrees l r = Just $ QBoth l r


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
