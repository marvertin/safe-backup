{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Yaba.Process.SlicoutBuilder (
  buildBackup,
  sizeToBackup,
  countsToBackup,
) where


import           Control.Arrow
import           Data.List                   (mapAccumL)
import qualified Data.Map                    as M
import           Data.Maybe
import           Debug.Trace
import           Debug.Trace
import           DirScan                     (RevPath, pth)
import           Dump
import           Lib
import           System.Directory.Tree
import           TurboWare
import           Types
import           Yaba.Data.Differences
import           Yaba.Data.Lodree
import           Yaba.Data.Slicout
import           Yaba.Process.TreeComparator


mapCall :: (RevPath -> a -> Slicout ) -> RevPath -> [(FileName, a)] -> [Slicout]
mapCall fce revpath = map (uncurry fce . (first (:revpath)))


buildBackup :: Lodree -> Lodree ->  FileName -> Maybe (Differences, Slicout)
buildBackup sliceLodree surceLodree outputDir =
  let
      sliceHashes = createMapOfHashes sliceLodree
      sourceHashes = createMapOfHashes surceLodree
      currentSliceHashes = createMapOfHashes (currentLodree sliceLodree)

      makePaths :: Hash -> Paths
      makePaths hash =
        let find  set = maybe [] fst (M.lookup hash set)
        in Paths (find sourceHashes) (find currentSliceHashes) (find sliceHashes)

      bFromLodree :: RevPath -> Lodree -> Slicout
      bFromLodree revpath@(name:_) lodree =
        let hash = hashLodree lodree
        in
         case M.lookup hash sliceHashes of
          Nothing  -> let paths = makePaths hash
                      in  case mustInsert revpath paths of
                            Nothing ->
                              case lodree of
                                LFile Ree{rsize, rtime} _   ->  File name (Insert rsize rtime)
                                LDir _ list -> Dir name (mapCall bFromLodree revpath list)  -- Dir name (map (uncurry bFromLodree) list)
                            Just pathToLink -> File name $ Link ("/" ++ outputDir ++ pathToLink) $ Info hash paths lodree
          Just (path: _, lodree2) -> File name $ Link path $ Info hash (makePaths hash) lodree2

      bFromDifferences :: RevPath -> Differences -> Slicout
      bFromDifferences (name:_) (QLeft lodree)   = let hash = hashLodree lodree in File name $ Delete $ Info hash (makePaths hash) lodree
      bFromDifferences revpath (QRight lodree)  = bFromLodree revpath lodree
      bFromDifferences revpath (QBoth _ lodree) = bFromLodree revpath lodree
      bFromDifferences revpath@(name:_) (QDir list) = Dir name (mapCall bFromDifferences  revpath list)

      diff :: Maybe Differences
      diff = compareTrees (currentLodree sliceLodree) surceLodree

  in  fmap (\di -> (di, bFromDifferences [outputDir] di)) diff

mustInsert :: RevPath -> Paths -> Maybe FilePath
mustInsert _ (Paths [] _ _) = Nothing -- impossible
mustInsert revpath (Paths (itMustInsert:_) _ _) =
    let path = namesToPath . init $ revpath
    in if path == itMustInsert then Nothing
                               else Just itMustInsert

sizeToBackup :: Slicout -> Integer
sizeToBackup bt = sum $ fmap mapa bt
   where mapa (Insert sz _) = sz
         mapa _             = 0

countsToBackup :: Slicout -> Integer
countsToBackup bt = sum $ fmap mapa bt
  where mapa (Insert _ _) = 1
        mapa _            = 0

type MapByHash = M.Map Hash [FileName]



{-
    toDump x = [tostr x]
      where tostr (Insert hash) = "Insert  " ++ toHexStr hash
            toStr x = show x
    toDumpS = tostr
      where tostr (Insert hash) = "Insert  " ++ toHexStr hash
            toStr x = show x


                  replaceRedundantNewFiles :: Slicout -> Slicout
                  replaceRedundantNewFiles =
                      let
                        --zz :: MapByHash -> Slicout -> (MapByHash, Slicout)
                        repla :: [FileName] -> MapByHash -> Slicout -> (MapByHash, Slicout)
                        repla path hm this@(File name (Insert hash)) =
                            case M.lookup hash hm of
                              Nothing   -> (M.insert hash (name:path) hm, this)
                              Just path -> (hm, File name ( Link Newl (namesToPath path) hash (makePaths hash) emptyLodree))
                      -- mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
                        repla path hm (Dir name list) = let
                           (hm2 , list2) = mapAccumL (repla (name:path)) hm list
                           in (hm2, Dir name list2)
                        repla _ hm x = (hm, x)
                      in snd . repla [] M.empty


-}
