module BackupTreeBuilder (
  buildFromLodree,
  buildFromDirCompare
) where


import           Data.Maybe
import           Lib
import           LogicalDirTree
import           System.Directory.Tree
import           TreeComparator
import           TurboWare
import           Types

data Cmd = Insert | Delete | Link deriving (Show)

type BackupTree = DirTree Cmd

buildFromLodree :: FileName -> Lodree  -> BackupTree
buildFromLodree fileName (LFile _) = File fileName Insert
buildFromLodree fileName (LDir _ list) = Dir fileName (map (uncurry buildFromLodree) list)

buildFromDirCompare :: FileName -> DirCompare -> BackupTree
buildFromDirCompare name (QLeft lodree)   = File name Delete
buildFromDirCompare name (QRight lodree)  = buildFromLodree name lodree
buildFromDirCompare name (QBoth _ lodree) = buildFromLodree name lodree
buildFromDirCompare name (QDir list) =  Dir name (map (uncurry buildFromDirCompare) list)
