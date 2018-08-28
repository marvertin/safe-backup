module Backup (
  readBackupDir,
  backup
) where

import           BackupTreeBuilder
import           Data.List
import           Dump
import           Lib
import           LogicalDirTree
import           SourceTree
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           TurboWare
import           Types
import           YabaDirTree
import           YabaFileContent

import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           System.IO             (hFlush, stdout)
import           YabaFileContent


isSliceName :: FileName -> Bool
isSliceName = isSuffixOf yabaSliceSuffix

readBackupDir :: FilePath -> IO Lodree
readBackupDir backupRoot = do
  yabaDirNames <-  (sort . filter isSliceName) <$> listDirectory backupRoot
  putStrLn $ "Reading " ++ show (length yabaDirNames) ++ " slices allredy backed up"
  yabaDirs <- mapM (\name -> deAnchore <$> readYabaDir (backupRoot ++ "/" ++ name)) yabaDirNames
  let rootLodree = mergesToLodree emptyLodree yabaDirs
  return rootLodree

--maintree = "maintree"

writeBackup :: AnchoredBackupTree -> [(FileName, FilePath)] -> IO ()
writeBackup x sourceTrees = do
    putStrLn $ "jsem v writeBackup"
    hFlush stdout
    -- nasledujici prikaz buh vi proc dlouho trva
    let (base :/ d@(Dir yabadir _)) = x
    putStrLn $ "Writing backu slice: " ++ yabadir
    hFlush stdout
    mapM ( \(treeName, treePath) -> do
       putStrLn $ "Writing tree: " ++ treePath ++ " ==> " ++ (yabadir </> treeName)
       putStrLn $ unlines $ dirTreeToStringList (Just . show) $ d
       -- musíme umazat adresář yaba a také adresář kořene
       let kolikSmazat = 1 + length yabadir + 1 + length treeName + length base
       writeDirectoryWith (writeFileToBackup kolikSmazat treePath) x
      ) sourceTrees
    return ()
  where
    writeFileToBackup :: Int -> FilePath -> FilePath -> Cmd -> IO ()
    writeFileToBackup kolikSmazat sourceOfMainTreeDir path (Insert _) =
       let odkud = sourceOfMainTreeDir ++ drop kolikSmazat path
       in do
        putStrLn $  "kopy file: " ++ odkud ++ " --> " ++ path
        copyFile odkud path
    writeFileToBackup _ _ path cmd = do
        let (dir, file) = splitFileName path
        let cesta = dir </> ("~TOD~" ++ file) ++ ".yaba"
        putStrLn $ "budeme resit: " ++ cesta
            -- ++ unJabaContent (convertToJabaContent cmd)
        writeFile cesta (unJabaContent (convertToJabaContent cmd))


backup :: FilePath -> [(FileName, FilePath)] ->  IO ()
backup backupDir sourceTrees = do
  newYabaDir <- nextBackupDir
  lodreeBackupAll <- readBackupDir backupDir
  let lodreeBackupCurrent = currentLodree lodreeBackupAll
  putStrLn $  "Reading " ++ show (length sourceTrees) ++ " source trees"
  lodreeSourceAllNodes <- makeLDir <$> mapM ( \(treeName, treePath) -> do
                    lodreeSourceOneNode <- readSourceTree treePath
                    return (treeName, lodreeSourceOneNode)
                   ) sourceTrees
  --lodreeSourceOneNode <- readSourceTree sourceOfMainTreeDir
  -- let lodreeSourceAllNodes = LDir emptyDRee [(maintree, lodreeSourceOneNode)]
  putStrLn $ "Building new backup slice: " ++ newYabaDir
  case buildBackup lodreeBackupAll lodreeSourceAllNodes newYabaDir of
    Nothing ->  putStrLn $ "Nothing to backup: "
    Just backupDirTree -> do
       putStrLn $ "Writing backup to: " ++ backupDir
       writeBackup (backupDir :/ backupDirTree) sourceTrees
  -- return ()

nextBackupDir :: IO FilePath
nextBackupDir = do
  now <- getCurrentTime
  return $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H-%M-%SZ")) now
    ++ yabaSliceSuffix

convertToYaba :: Cmd -> YabaFileContent
convertToYaba (BackupTreeBuilder.LogicalLink x) = YabaFileContent.LogicalLink x
convertToYaba (BackupTreeBuilder.PhysicalLink x) = YabaFileContent.PhysicalLink x
convertToYaba (BackupTreeBuilder.NewLink x) = YabaFileContent.PhysicalLink x
convertToYaba (BackupTreeBuilder.Delete) = YabaFileContent.Delete

convertToJabaContent :: Cmd -> JabaContent
convertToJabaContent = formatYabaFile . convertToYaba


ba :: IO ()
ba = do
  let backupDir = "./test/data/case3/backup"
  let sourceOfMainTree = "./test/data/case3/source-of-maintree"
  backup backupDir [("maintree", sourceOfMainTree)]
