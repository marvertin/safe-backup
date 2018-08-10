-- {-# LANGUAGE TupleSeRecordWildCards #-}

module TreeComparator (
  yy,
  compareTrees,
  dirCompareToStringList,

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


dirCompareToStringList :: DirCompare -> [String]

dirCompareToStringList (QLeft (LFile _)) = ["- . "]
dirCompareToStringList (QLeft (LDir _ _)) = ["- / "]
dirCompareToStringList (QRight (LFile _)) = ["+ . "]
dirCompareToStringList (QRight (LDir _ _)) = ["+ / "]
dirCompareToStringList (QBoth LFile {} LFile {}) = ["~ .."]
dirCompareToStringList (QBoth LFile {} LDir {}) = ["~ ./"]
dirCompareToStringList (QBoth LDir {} LFile {}) = ["~ /."]
dirCompareToStringList (QBoth LDir {} LDir {}) = ["~ //"]
dirCompareToStringList (QDir items) = ("       " ++) <$> (items >>= todump)

   where
      todump :: (FileName, DirCompare) -> [String]
      todump (filename, dc@(QDir dir)) = ("/    " ++ filename) : dirCompareToStringList dc
      todump (filename, dc) = appendToFirst (" " ++ filename) (dirCompareToStringList dc)


prependToFirst :: [a] -> [[a]] -> [[a]]
prependToFirst [] []     = []
prependToFirst x []      = [x]
prependToFirst x (y: ys) = (x ++ y) : ys

appendToFirst :: [a] -> [[a]] -> [[a]]
appendToFirst [] []     = []
appendToFirst x []      = [x]
appendToFirst x (y: ys) = (y ++ x) : ys


l x = (x, QLeft emptyLodree)
r x = (x, QRight emptyLodree)
o x = (x, QBoth emptyLodree emptyLodree)
d x y = (x, QDir y)

x = QDir [l "a", r "z", o "n", d "A" [ l "aa", l "bb"], d "N" [o "nn", d "NN" [d "NNN" [d "NNNN" [r "yyyy"]]]] ]

yy = do
  putStrLn $ unlines $ dirCompareToStringList x
