{-# LANGUAGE RecordWildCards #-}

module Hashpairing (
      --createFhysicalHashMap
    flattenRees,
    flattenLodrees,
    createFhysicalHashMap,
    createLogicalHashMap,

) where

import qualified Data.Map       as M
import           Lib
import           LogicalDirTree
import           TurboWare
import           Types


createFhysicalHashMap :: Lodree -> M.Map Hash FilePath
createFhysicalHashMap lodree = M.fromList $
  map (\Ree{..} -> (hash, physPathx)) $ flattenRees lodree

createLogicalHashMap :: Lodree -> M.Map Hash FilePath
createLogicalHashMap lodree = M.fromList $
  map (\(path, (LDir DRee{..} _)) -> (dhash, path)) $ flattenLodrees lodree


flattenRees :: Lodree -> [Ree]
flattenRees = fla []
  where
    fla reslist (LFile ree)  = ree : reslist
    fla reslist (LDir _ sez) = (snd <$> sez) >>= fla reslist

flattenLodrees :: Lodree -> [(FilePath, Lodree)]
flattenLodrees = fla [] "/"
  where
    fla :: [(FilePath, Lodree)] -> FilePath  -> Lodree -> [(FilePath, Lodree)]
    --fla reslist path q@(LFile _)  = (path,  q) : reslist
    fla reslist path q@(LFile _)  = reslist
    fla reslist path q@(LDir _ sez) = (path, q) :
        (sez >>= (\(p, lodree) -> fla reslist (path ++ "/" ++ p) lodree))
