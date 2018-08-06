module DirTreeCompare
    ( q
    ) where

import Lib
import DirTree
import System.Directory.Tree
import System.Directory
import           Text.Printf          (printf)

import           Crypto.Hash.SHA1     (hashlazy)


data QType = QFile | QDir
data CurrInfo = CurrNo | CurrJust (Hash, FileSize, QType, [DirTree2])
data BackInfo = BackNo | BackJust (Hash, FileSize, QType)
type DirTree2   = (FileName, CurrInfo, BackInfo)


compareFileOrDirList :: ([DirTree FileInfo], [DirTree FileInfo]) -> [DirTree2]
compareFileOrDirList _ = []

compareTrees :: DirTree FileInfo -> DirTree FileInfo -> DirTree2
compareTrees (File n1 (path1, size1, hash1)) (File n2 (path2, size2, hash2))
 | n1 == n2 = (n1, CurrJust (hash1, size1, QFile, []), BackJust (hash2, size2, QFile))
compareTrees q1@(Dir n1 content1) q2@(Dir n2 content2)
 | n1 == n2 = (n1, CurrJust ("hh", 0, QDir, compareFileOrDirList (content1, content2))
                 , BackJust ("hh", 0, QDir))

-- q :: IO ()
-- q = do
--  (base :/ d2) <- readSourceDir  "./"
--  putStrLn $ unlines $ zastringuj d2
