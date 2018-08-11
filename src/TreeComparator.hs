{-# LANGUAGE InstanceSigs #-}

module TreeComparator (
  compareTrees,
  toDump,
) where


import           Data.Maybe
import           Lib
import           LogicalDirTree

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
     compareDirs Nothing Nothing   = error "Imposible has happend! No dir no file"
     compareDirs (Just l) Nothing  = Just$ QLeft l
     compareDirs Nothing (Just r)  = Just$ QRight r
     compareDirs (Just l) (Just r) = compareTrees l r
compareTrees l r = Just $ QBoth l r

instance Dumpable DirCompare where
  toDump :: DirCompare -> [String]

  toDump (QLeft (LFile _)) = ["- . "]
  toDump (QLeft (LDir _ _)) = ["- / "]
  toDump (QRight (LFile _)) = ["+ . "]
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


prependToFirst :: [a] -> [[a]] -> [[a]]
prependToFirst [] []     = []
prependToFirst x []      = [x]
prependToFirst x (y: ys) = (x ++ y) : ys

appendToFirst :: [a] -> [[a]] -> [[a]]
appendToFirst [] []     = []
appendToFirst x []      = [x]
appendToFirst x (y: ys) = (y ++ x) : ys
