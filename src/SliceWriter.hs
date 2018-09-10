{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module SliceWriter (
  writeBackup,
  countCounters
)  where


import           Control.Arrow
import           Control.Monad
import           Data.Counter
import           Data.List
import qualified Data.Map              as M
import           Data.Time.Clock
import           System.Directory
import           System.Directory.Tree
import           System.Exit
import           System.FilePath
import           System.IO
import           Text.Printf


import           BackupTreeBuilder
import           Config
--  import           DirScan
import           Dump
--  import           Ignorances
import           Lib
--  import           Lodree
import           Log
import           Slice
--  import           SliceNameStrategy
import qualified Data.ByteString.Lazy  as BS
import           TurboWare
import           Types

-- | Write backu. returns (count of copies files, sizue of set files, count of metafiles)
writeBackup :: Log -> AnchoredBackupTree -> ForestDef ->  IO (AnchoredDirTree (Int, Integer, Int))
writeBackup lo abt@(base :/ bt@(Dir newSliceName _)) forest = do
        let (Dir _ subdirs) = (first replaceWithSorucePath) <$> zipPaths ("" :/ bt)
        writeDirectoryWith writeFileToBackup (base :/ (Dir (replaceVerticalToSlashes newSliceName) subdirs))
    where
      replaceWithSorucePath :: FilePath -> FilePath
      replaceWithSorucePath fp = let
           (treeName, path) = break (=='/') . drop (length newSliceName + 1) . replaceBacklashesToSlashes $ fp
           in case tdPath <$> find ((==treeName) . tdName) forest of
             Nothing ->  error $ "Imposible has happened! \"" ++ treeName ++ "\" not found in " ++ show forest ++ " the rest is \"" ++ path ++ "\""
             Just root -> root ++ path

      --writeFileToBackup :: Int -> FilePath -> FilePath -> Cmd -> IO ()
      writeFileToBackup :: FilePath -> (FilePath, Cmd) -> IO (Int, Integer, Int)
      writeFileToBackup destPath (sourcePath, (Insert _))  = do
           lo Debug $ printf "copy file: \"%s\" --> \"%s\"" sourcePath destPath
           BS.readFile sourcePath >>= BS.writeFile destPath
           (1,,0) <$> getFileSize destPath

      writeFileToBackup path (_, cmd) = do
           let (dir, file) = splitFileName path
           let cesta = dir </> (yabaFilePrefix cmd ++ file) ++ ".yaba"
           lo Debug $ printf  "create meta: \"%s\": %s" cesta (showCmd cmd )
           writeFile cesta (formatCmd cmd)
           return (0,0,1)

showCmd :: Cmd ->  String
showCmd (BackupTreeBuilder.Insert {}) = "<insert>"
showCmd (BackupTreeBuilder.Delete {}) = "<delete>"
showCmd (BackupTreeBuilder.Link fp _) = "<link \"" ++ fp ++  "\" >"

title x = ["", "----- " ++ x ++ " ---------------------------"]

formatCmd :: Cmd -> String
formatCmd cmd = unlines $ concat $ [
     (formatMetaFileHeader . convertToSliceCmd) cmd,
     title "Operation",
     [yabaFilePrefix cmd],
     case cmd of
           Link _ info                   -> formatInfo info
           BackupTreeBuilder.Delete info -> formatInfo info
  ]

formatInfo :: Info -> [String]
formatInfo (Info hash Paths{..} lodree)  = concat [
    title "Hash" ,
    [toHexStr hash],
    [show hash],
    title "Source paths",
    pathsNew,
    title "Last slice paths",
    pathsLast,
    title "History paths",
    pathsHistory,
    title "Tree",
    toDump lodree
   ]

countCounters :: BackupTree -> Counter String Int
countCounters = count . (foldMap (return . yabaFilePrefix))


yabaFilePrefix :: Cmd -> String
yabaFilePrefix (BackupTreeBuilder.Delete (Info _ (Paths {pathsNew=[]}) _ ))= "~DELETE~"
yabaFilePrefix (BackupTreeBuilder.Delete _)= "~MOVE-AWAY~"
yabaFilePrefix (BackupTreeBuilder.Link _ (Info _ (Paths {pathsHistory=[]}) _ ))= "~N-LINK~"
yabaFilePrefix (BackupTreeBuilder.Link _ _)   = "~LINK~"
yabaFilePrefix (BackupTreeBuilder.Insert{})   = "~INSERT~"
-- yabaFilePrefix _ = "~IMPOSSIBLE~"

convertToSliceCmd :: Cmd -> SliceCmd
convertToSliceCmd (BackupTreeBuilder.Link  x _) = Slice.PhysicalLink x
convertToSliceCmd (BackupTreeBuilder.Delete _ ) = Slice.Delete
