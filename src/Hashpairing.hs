{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}

module Hashpairing (
    -- flattenRees,
    -- flattenLodrees,
    createMapOfHashes,

) where

import           Data.Function
import           Data.List
import qualified Data.Map      as M
import           Lib
import           Lodree
import           TurboWare
import           Types




createMapOfHashes :: Lodree -> M.Map Hash FilePath
createMapOfHashes lodree = M.fromList $ -- last winns
  map (\(path, lodree) -> (rhash . ree $ lodree , path)) $
   (reverse . sortBy (compare `on` fst) $ flattenLodrees lodree)

createMapOfHashes' :: Lodree -> M.Map Hash ([FilePath], Lodree)
createMapOfHashes' lodree =
  let
      list :: [(Hash, (FilePath, Lodree))]
      list =
            map (\(path, lodree) -> (rhash . ree $ lodree , (path, lodree))) $
            (sortBy (flip compare `on` fst) $ flattenLodrees lodree)

      grouped :: [[(Hash, (FilePath, Lodree))]]
      grouped = groupBy ((==) `on` fst) list

      organized :: [(Hash, ([FilePath], Lodree))]
      organized = fmap konv grouped
  in M.fromList organized


konv :: [(Hash, (FilePath, Lodree))] -> (Hash, ([FilePath], Lodree))
konv x = let
              pathList :: [FilePath]
              pathList = (fst . snd) <$> x
              hash :: Hash
              hash = fst . head $ x
              lodree :: Lodree
              lodree = snd . snd . head $ x
         in (hash, (pathList, lodree))

flattenLodrees :: Lodree -> [(FilePath, Lodree)]
flattenLodrees = fla [] ""
  where
    fla :: [(FilePath, Lodree)] -> FilePath  -> Lodree -> [(FilePath, Lodree)]
    --fla reslist path q@(LFile _)  = (path,  q) : reslist
    fla reslist path q@(LFile _)  = (path, q) : reslist
    fla reslist path q@(LDir _ sez) = (path, q) :
        (sez >>= (\(p, lodree) -> fla reslist (path ++ "/" ++ p) lodree))
        -- TODO  eliminate O(n^2)

instance Dumpable (M.Map Hash FilePath) where
   toDump m = map (\(k,v) -> toHexStr k ++ " = " ++ v) (M.toList m)
