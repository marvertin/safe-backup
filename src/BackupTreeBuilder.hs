{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module BackupTreeBuilder (
  buildBackup,
  sizeToBackup,
  countsToBackup,
) where


import           Control.Arrow
import           Data.List              (mapAccumL)
import qualified Data.Map               as M
import           Data.Maybe
import           Debug.Trace
import           Debug.Trace
import           DirScan                (RevPath, pth)
import           Dump
import           Hashpairing
import           Lib
import           SliceToLodree
import           System.Directory.Tree
import           TreeComparator
import           TurboWare
import           Types
import           Yaba.Data.Differences
import           Yaba.Data.Lodree
import           Yaba.Data.SliceWritten


mapCall :: (RevPath -> a -> BackupTree ) -> RevPath -> [(FileName, a)] -> [BackupTree]
mapCall fce revpath = map (uncurry fce . (first (:revpath)))


buildBackup :: Lodree -> Lodree ->  FileName -> Maybe (DirCompare, BackupTree)
buildBackup sliceLodree surceLodree outputDir =
  let
      sliceHashes = createMapOfHashes' sliceLodree
      sourceHashes = createMapOfHashes' surceLodree
      currentSliceHashes = createMapOfHashes' (currentLodree sliceLodree)

      makePaths :: Hash -> Paths
      makePaths hash =
        let find  set = maybe [] fst (M.lookup hash set)
        in Paths (find sourceHashes) (find currentSliceHashes) (find sliceHashes)

      bFromLodree :: RevPath -> Lodree -> BackupTree
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

      bFromDirCompare :: RevPath -> DirCompare -> BackupTree
      bFromDirCompare (name:_) (QLeft lodree)   = let hash = hashLodree lodree in File name $ Delete $ Info hash (makePaths hash) lodree
      bFromDirCompare revpath (QRight lodree)  = bFromLodree revpath lodree
      bFromDirCompare revpath (QBoth _ lodree) = bFromLodree revpath lodree
      bFromDirCompare revpath@(name:_) (QDir list) = Dir name (mapCall bFromDirCompare  revpath list)

      diff :: Maybe DirCompare
      diff = compareTrees (currentLodree sliceLodree) surceLodree

  in  fmap (\di -> (di, bFromDirCompare [outputDir] di)) diff

mustInsert :: RevPath -> Paths -> Maybe FilePath
mustInsert _ (Paths [] _ _) = Nothing -- impossible
mustInsert revpath (Paths (itMustInsert:_) _ _) =
    let path = namesToPath . init $ revpath
    in if path == itMustInsert then Nothing
                               else Just itMustInsert

sizeToBackup :: BackupTree -> Integer
sizeToBackup bt = sum $ fmap mapa bt
   where mapa (Insert sz _) = sz
         mapa _             = 0

countsToBackup :: BackupTree -> Integer
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


                  replaceRedundantNewFiles :: BackupTree -> BackupTree
                  replaceRedundantNewFiles =
                      let
                        --zz :: MapByHash -> BackupTree -> (MapByHash, BackupTree)
                        repla :: [FileName] -> MapByHash -> BackupTree -> (MapByHash, BackupTree)
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
