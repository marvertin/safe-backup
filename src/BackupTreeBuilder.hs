module BackupTreeBuilder (
  buildBackup,
  AnchoredBackupTree,
  Cmd(..),
) where


import           Data.List             (mapAccumL)
import qualified Data.Map              as M
import           Data.Maybe
import           Debug.Trace
import           Hashpairing
import           Lib
import           LogicalDirTree
import           System.Directory.Tree
import           TreeComparator
import           TurboWare
import           Types

data Cmd = Insert Hash | Delete | LogicalLink FilePath | PhysicalLink FilePath | NewLink FilePath deriving (Show)

type BackupTree = DirTree Cmd
type AnchoredBackupTree = AnchoredDirTree Cmd


buildBackup :: Lodree -> Lodree ->  FileName -> Maybe BackupTree
buildBackup blodree slodree outputDir =
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
      -- diff = trace ("\n\nslodree: " ++ show slodree ++ "\n\ncurrentLodre eblodree: " ++ show (currentLodree blodree) ++ "\n\n")
      diff = compareTrees (currentLodree blodree) slodree

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
            Just path -> (hm, File name (NewLink (namesToPath path)))
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
