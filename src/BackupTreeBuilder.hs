module BackupTreeBuilder (
  buildBackup,
  AnchoredBackupTree,
  Cmd(..),
  LinkType(..)
) where


import           Data.List             (mapAccumL)
import qualified Data.Map              as M
import           Data.Maybe
import           Debug.Trace
import           Hashpairing
import           Lib
import           Lodree
import           SliceToLodree
import           System.Directory.Tree
import           TreeComparator
import           TurboWare
import           Types

data LinkType = Newl | Movel deriving (Show)
data Cmd = Insert Hash | Delete | Link LinkType Hash [FilePath] Lodree  deriving (Show)
data Paths = Paths { pathsNew :: [FilePath], pathsLast:: [FilePath], pathsHistory :: [FilePath] }

type BackupTree = DirTree Cmd
type AnchoredBackupTree = AnchoredDirTree Cmd


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

      hashing :: FileName -> Lodree -> BackupTree
      hashing name lodree =
        let hash = hashLodree lodree
        in
         case M.lookup hash sliceHashes of
          Nothing              ->   bFromLodree name lodree
          Just (paths, lodree) -> File name (Link Movel hash paths lodree)


      bFromLodree :: FileName -> Lodree  -> BackupTree
      --bFromLodree name lodree@(LFile _) = File name (Insert (hashLodree lodree))
      --bFromLodree name (LDir _ list) = Dir name (map (uncurry bFromLodree) list)
      -- TODO Také vyřešit hašování
      bFromLodree name lodree@(LFile _) = File name (Insert (hashLodree lodree))
      bFromLodree name (LDir _ list) = Dir name (map (uncurry hashing) list)

      bFromDirCompare :: FileName -> DirCompare -> BackupTree
      bFromDirCompare name (QLeft lodree)   = File name Delete
      bFromDirCompare name (QRight lodree)  = hashing name lodree
      bFromDirCompare name (QBoth _ lodree) = hashing name lodree
      bFromDirCompare name (QDir list) =  Dir name (map (uncurry bFromDirCompare) list)
      -- diff = trace ("\n\nsurceLodree: " ++ show surceLodree ++ "\n\ncurrentLodre esliceLodree: " ++ show (currentLodree sliceLodree) ++ "\n\n")
      diff = compareTrees (currentLodree sliceLodree) surceLodree

  in  (replaceRedundantNewFiles . bFromDirCompare outputDir) <$> diff

type MapByHash = M.Map Hash [FileName]

replaceRedundantNewFiles :: BackupTree -> BackupTree
replaceRedundantNewFiles =
    let
      --zz :: MapByHash -> BackupTree -> (MapByHash, BackupTree)

      repla :: [FileName] -> MapByHash -> BackupTree -> (MapByHash, BackupTree)
      repla path hm this@(File name (Insert hash)) =
          case M.lookup hash hm of
            Nothing   -> (M.insert hash (name:path) hm, this)
            Just path -> (hm, File name ( Link Newl hash [namesToPath path] emptyLodree))
    -- mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
      repla path hm (Dir name list) = let
         (hm2 , list2) = mapAccumL (repla (name:path)) hm list
         in (hm2, Dir name list2)
      repla _ hm x = (hm, x)
    in snd . (repla [] M.empty)


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
