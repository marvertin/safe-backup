module BackupTreeBuilder (
  buildFromLodree,
  buildFromDirCompare,
  buildBackup,
) where


import qualified Data.Map              as M
import           Data.Maybe
import           Hashpairing
import           Lib
import           LogicalDirTree
import           System.Directory.Tree
import           TreeComparator
import           TurboWare
import           Types

data Cmd = Insert | Delete | LogicalLink FilePath | PhysicalLink FilePath deriving (Show)

type BackupTree = DirTree Cmd

buildFromLodree :: FileName -> Lodree  -> BackupTree
buildFromLodree fileName (LFile _) = File fileName Insert
buildFromLodree fileName (LDir _ list) = Dir fileName (map (uncurry buildFromLodree) list)

buildFromDirCompare :: FileName -> DirCompare -> BackupTree
buildFromDirCompare name (QLeft lodree)   = File name Delete
buildFromDirCompare name (QRight lodree)  = buildFromLodree name lodree
buildFromDirCompare name (QBoth _ lodree) = buildFromLodree name lodree
buildFromDirCompare name (QDir list) =  Dir name (map (uncurry buildFromDirCompare) list)

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
      bFromLodree fileName (LFile _) = File fileName Insert
      bFromLodree fileName (LDir _ list) = Dir fileName (map (uncurry bFromLodree) list)

      bFromDirCompare :: FileName -> DirCompare -> BackupTree
      bFromDirCompare name (QLeft lodree)   = File name Delete
      bFromDirCompare name (QRight lodree)  = hashing name lodree
      bFromDirCompare name (QBoth _ lodree) = hashing name lodree
      bFromDirCompare name (QDir list) =  Dir name (map (uncurry bFromDirCompare) list)

      diff = compareTrees (currentLodree blodree) slodree

  in  bFromDirCompare "KOREN-BEKAPU" (fromJust diff)
