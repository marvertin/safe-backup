{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE RecordWildCards #-}

module Baup.Process.TreeComparator (
  compareTrees,
) where


import           Data.Maybe
import           Util.Lib
import           Util.TurboWare
import           Util.Types
import           Baup.Data.Differences
import           Baup.Data.Lodree

compareTrees :: Lodree -> Lodree -> Maybe Differences
compareTrees l r
  | hashLodree l == hashLodree r = Nothing
compareTrees (LDir _ ls) (LDir _ rs) = let
   list :: [(FileName, Maybe (FileName, Lodree), Maybe (FileName, Lodree))]
   list = zipMaybe fst fst ls rs
   compareDirs2 (fileName, l, r) =  fmap ((,) fileName) (compareDirs (snd <$> l) (snd <$> r))
  in Just $ QDir (mapMaybe compareDirs2 list)
  where
     compareDirs :: Maybe Lodree -> Maybe Lodree -> Maybe Differences
     compareDirs Nothing Nothing   = error "Imposible has happend! No dir, no file"
     compareDirs (Just l) Nothing  = Just$ QLeft l
     compareDirs Nothing (Just r)  = Just$ QRight r
     compareDirs (Just l) (Just r) = compareTrees l r
compareTrees l r = Just $ QBoth l r
