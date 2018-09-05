{-# LANGUAGE NamedFieldPuns #-}

module BackupTreeBuilder (
  buildBackup,
  AnchoredBackupTree,
  Cmd(..),
  LinkType(..),
  Paths(..)
) where


import           Control.Arrow
import           Data.List             (mapAccumL)
import qualified Data.Map              as M
import           Data.Maybe
import           Debug.Trace
import           Debug.Trace
import           DirScan               (RevPath, pth)
import           Hashpairing
import           Lib
import           Lodree
import           SliceToLodree
import           System.Directory.Tree
import           TreeComparator
import           TurboWare
import           Types

data LinkType = Newl | Movel deriving (Show)
data Cmd = Insert Hash | Delete Hash Paths Lodree | Link LinkType FilePath Hash Paths Lodree  deriving (Show)
data Paths = Paths { pathsNew :: [FilePath], pathsLast:: [FilePath], pathsHistory :: [FilePath] }  deriving (Show)

type BackupTree = DirTree Cmd
type AnchoredBackupTree = AnchoredDirTree Cmd

mapCall :: (RevPath -> a -> BackupTree ) -> RevPath -> [(FileName, a)] -> [BackupTree]
mapCall fce revpath = map (uncurry fce . (first (:revpath)))


buildBackup :: Lodree -> Lodree ->  FileName -> Maybe BackupTree
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
          Nothing  ->  case lodree of
                        LFile Ree{rhash} _   ->  let  paths = (makePaths rhash)
                             in File name $ if mustInsert revpath paths then Insert rhash
                                                                        else Link Newl (namesToPath revpath) hash paths lodree
                        LDir _ list -> Dir name (mapCall bFromLodree revpath list)  -- Dir name (map (uncurry bFromLodree) list)
          Just (path: _, lodree) -> File name (Link Movel path hash (makePaths hash) lodree)

      bFromDirCompare :: RevPath -> DirCompare -> BackupTree
      bFromDirCompare (name:_) (QLeft lodree)   = let hash = hashLodree lodree in File name (Delete hash (makePaths hash) lodree)
      bFromDirCompare revpath (QRight lodree)  = bFromLodree revpath lodree
      bFromDirCompare revpath (QBoth _ lodree) = bFromLodree revpath lodree
      bFromDirCompare revpath@(name:_) (QDir list) = Dir name (mapCall bFromDirCompare  revpath list)
      -- diff = trace ("\n\nsurceLodree: " ++ show surceLodree ++ "\n\ncurrentLodre esliceLodree: " ++ show (currentLodree sliceLodree) ++ "\n\n")
      diff = compareTrees (currentLodree sliceLodree) surceLodree

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

  in  (bFromDirCompare [outputDir]) <$> diff

mustInsert :: RevPath -> Paths -> Bool
mustInsert _ (Paths [] _ _) = True -- impossible
mustInsert revpath (Paths (x:_) _ _) =
    let path = namesToPath . init $ revpath
    in path == x


type MapByHash = M.Map Hash [FileName]




instance Dumpable Cmd where
    toDump x = [ "**" ++ show x ]
    toDumpS = tostra
      where
          tostra (Insert hash) = "Insert  " ++ toHexStr hash
          tostra x             = show x

{-
    toDump x = [tostr x]
      where tostr (Insert hash) = "Insert  " ++ toHexStr hash
            toStr x = show x
    toDumpS = tostr
      where tostr (Insert hash) = "Insert  " ++ toHexStr hash
            toStr x = show x
-}
