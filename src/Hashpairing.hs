{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}

module Hashpairing (
      --createFhysicalHashMap
    flattenRees,
    flattenLodrees,
    createFhysicalHashMap,
    createMapOfHashes,

) where

import           Data.Function
import           Data.List
import qualified Data.Map      as M
import           Lib
import           Lodree
import           TurboWare
import           Types


createFhysicalHashMap :: Lodree -> M.Map Hash FilePath
createFhysicalHashMap lodree = M.fromList $
  map (\Ree{..} -> (rhash, rphysPath)) $ flattenRees lodree

createMapOfHashes :: Lodree -> M.Map Hash FilePath
createMapOfHashes lodree = M.fromList $ -- last winns
  map (\(path, lodree) -> (rhash . ree $ lodree , path)) $
   (reverse . sortBy (compare `on` fst) $ flattenLodrees lodree)


flattenRees :: Lodree -> [Ree]
flattenRees = fla []
  where
    fla reslist (LFile ree)  = ree : reslist
    fla reslist (LDir _ sez) = ((snd <$> sez) >>= fla reslist)

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
