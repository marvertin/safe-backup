{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module SliceWriter (
  writeBackup,
  writeBackup2,
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
import           TurboWare
import           Types

writeBackup2 :: Log -> AnchoredBackupTree -> ForestDef ->  IO [AnchoredDirTree ()]
writeBackup2 lo abt@(base :/ bt@(Dir newSliceName _)) forest = do
  putStrLn $ "########### " ++ base ++ "###################"
  dump bt
  putStrLn $ "###################################"
  let xx@(Dir _ subdirs) = (first replaceWithSorucePath) <$> zipPaths ("" :/ bt)
  forM_ xx (putStrLn . show . fmap printCmd)
  sss <- writeDirectoryWith writeFileToBackup (base :/ (Dir (replaceVerticalToSlashes newSliceName) subdirs))
  return [sss]
    where
      replaceWithSorucePath :: FilePath -> FilePath
      replaceWithSorucePath fp = let
           (treeName, path) = break (=='/') . drop (length newSliceName + 1) . replaceBacklashesToSlashes $ fp
           in case tdPath <$> find ((==treeName) . tdName) forest of
             Nothing ->  error $ "Imposible has happened! \"" ++ treeName ++ "\" not found in " ++ show forest ++ " the rest is \"" ++ path ++ "\""
             Just root -> root ++ path

      --writeFileToBackup :: Int -> FilePath -> FilePath -> Cmd -> IO ()
      writeFileToBackup :: FilePath -> (FilePath, Cmd) -> IO ()
      writeFileToBackup destPath (sourcePath, (Insert _))  =
           -- putStrLn $ show kolikSmazat ++ "* " ++ path ++ " | " ++ odkud ++ " --> " ++ path
           lo Inf ("copy file: " ++ sourcePath ++ " --> " ++ destPath)
           -- copyFile odkud path
      writeFileToBackup path (_, cmd) = do
           let (dir, file) = splitFileName path
           let cesta = dir </> (yabaFilePrefix cmd ++ file) ++ ".yaba"
           lo Debug $ "create meta: " ++ cesta
               -- ++ unJabaContent (convertToJabaContent cmd)
           writeFile cesta (formatCmd cmd)

printCmd :: Cmd ->  String
printCmd (BackupTreeBuilder.Insert {}) = "<insert>"
printCmd (BackupTreeBuilder.Delete {}) = "<delete>"
printCmd (BackupTreeBuilder.Link fp _) = "<link \"" ++ fp ++  "\" >"


writeBackup :: Log -> AnchoredBackupTree -> ForestDef -> IO [AnchoredDirTree ()]
writeBackup lo abt' sourceTrees = do
    -- putStrLn $ "jsem v writeBackup"
    -- hFlush stdout
    -- nasledujici prikaz buh vi proc dlouho trva
    let (base :/ (Dir yabadir subdirlist1)) = abt'
    let abt = base :/ Dir (replaceVerticalToSlashes yabadir) subdirlist1
    mapM ( \(TreeDef treeName treePath _) -> do
       --putStrLn $ "Writing tree: " ++ treePath ++ " ==> " ++ (yabadir </> treeName)
       --putStrLn $ unlines $ dirTreeToStringList (Just . show) d
       -- musíme umazat adresář yaba a také adresář kořene
       let kolikSmazat = 1 + length yabadir + 1 + length treeName + length base
       writeDirectoryWith (writeFileToBackup kolikSmazat treePath) abt
      ) sourceTrees
  where
    writeFileToBackup :: Int -> FilePath -> FilePath -> Cmd -> IO ()
    writeFileToBackup kolikSmazat sourceOfMainTreeDir path (Insert _)  =
       let odkud = sourceOfMainTreeDir ++ drop kolikSmazat path
       in do
        -- putStrLn $ show kolikSmazat ++ "* " ++ path ++ " | " ++ odkud ++ " --> " ++ path
        lo Debug ("copy file: " ++ odkud ++ " --> " ++ path)
        copyFile odkud path
    writeFileToBackup _ _ path cmd = do
        let (dir, file) = splitFileName path
        let cesta = dir </> (yabaFilePrefix cmd ++ file) ++ ".yaba"
        lo Debug $ "create meta: " ++ cesta
            -- ++ unJabaContent (convertToJabaContent cmd)
        writeFile cesta (formatCmd cmd)

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
