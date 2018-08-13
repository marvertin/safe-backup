module BackupTreeBuilder (
  buildBackup,
) where


import           Data.List             (intercalate, mapAccumL)
import qualified Data.Map              as M
import           Data.Maybe
import           Hashpairing
import           Lib
import           LogicalDirTree
import           System.Directory.Tree
import           TreeComparator
import           TurboWare
import           Types

data Cmd = Insert Hash | Delete | LogicalLink FilePath | PhysicalLink FilePath | NewLink FilePath deriving (Show)

type BackupTree = DirTree Cmd


buildBackup :: Lodree -> Lodree-> BackupTree
buildBackup blodree slodree =
  let fileHashes = createFhysicalHashMap blodree
      dirHashes = createLogicalHashMap blodree

      hashing :: FileName -> Lodree -> BackupTree
      hashing name lodree =
         case M.lookup (hashLodree lodree) fileHashes of
          Nothing   ->  case M.lookup (hashLodree lodree) dirHashes of
                            Nothing   -> bFromLodree name lodree
                            Just path -> File name (LogicalLink path)
          Just path -> File name (PhysicalLink path)


      bFromLodree :: FileName -> Lodree  -> BackupTree
      bFromLodree fileName lodree@(LFile _) = File fileName (Insert (hashLodree lodree))
      bFromLodree fileName (LDir _ list) = Dir fileName (map (uncurry bFromLodree) list)

      bFromDirCompare :: FileName -> DirCompare -> BackupTree
      bFromDirCompare name (QLeft lodree)   = File name Delete
      bFromDirCompare name (QRight lodree)  = hashing name lodree
      bFromDirCompare name (QBoth _ lodree) = hashing name lodree
      bFromDirCompare name (QDir list) =  Dir name (map (uncurry bFromDirCompare) list)

      diff = compareTrees (currentLodree blodree) slodree

  in  replaceRedundantNewFiles (bFromDirCompare "KOREN-BEKAPU" (fromJust diff))

type MapByHash = M.Map Hash [FileName]

replaceRedundantNewFiles :: BackupTree -> BackupTree
replaceRedundantNewFiles =
    let
      --zz :: MapByHash -> BackupTree -> (MapByHash, BackupTree)

      repla :: [FileName] -> MapByHash -> BackupTree -> (MapByHash, BackupTree)
      repla path hm this@(File name (Insert hash)) =
          case M.lookup hash hm of
            Nothing   -> (M.insert hash (name:path) hm, this)
            Just path -> (hm, File name (NewLink (namesToPath path)))
    -- mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
      repla path hm (Dir name list) = let
         (hm2 , list2) = mapAccumL (repla path) hm list
         in (hm2, Dir name list2)
      repla _ hm x = (hm, x)
    in snd . (repla [] M.empty)
