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
import           TurboWare
import           Types
import           YabaDirTree
import           YabaFileContent

import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format


readBackupDir :: FilePath -> IO Lodree
readBackupDir backupRoot = do
  yabaDirNames <-  sort <$> listDirectory backupRoot
  print yabaDirNames
  yabaDirs <- mapM (\name -> deAnchore <$> readYabaDir (backupRoot ++ "/" ++ name)) yabaDirNames
  let rootLodree = mergesToLodree emptyLodree yabaDirs
  return rootLodree


writeBackup :: AnchoredBackupTree -> FileName -> IO AnchoredBackupTree
writeBackup x sourceOfMainTreeDir = do
    let (base :/ d) = x
    putStrLn $ "Budeme backupvat do3: " ++ base ++ " z " ++ sourceOfMainTreeDir
    putStrLn $ unlines $ dirTreeToStringList (Just . show) $
      d

    -- (_ :/ resfaile) <- writeJustDirs x
    -- print $ failures resfaile
    writeDirectoryWith (writeFileToBackup base) x
    return  x
  where
    writeFileToBackup :: FilePath -> FilePath -> Cmd -> IO ()
    writeFileToBackup odkudRoot path (Insert _) =
       let odkud = drop (length odkudRoot) path
       in do
        putStrLn $  path ++ " <- " ++ odkud
    writeFileToBackup _ path _ = do
        putStrLn $ "budeme resit: " ++ path


backup :: FilePath -> FilePath ->  IO AnchoredBackupTree
backup backupDir sourceOfMainTreeDir = do
  newYabaDir <- nextBackupDir
  lodreeBackupAll <- readBackupDir backupDir
  let lodreeBackupCurrent = currentLodree lodreeBackupAll
  lodreeSourceOneNode <- readSourceTree sourceOfMainTreeDir
  let lodreeSourceAllNodes = LDir emptyDRee [("maintree", lodreeSourceOneNode)]
  let backupDirTree = buildBackup lodreeBackupAll lodreeSourceAllNodes newYabaDir
  writeBackup (backupDir :/ backupDirTree) sourceOfMainTreeDir
  -- return ()

nextBackupDir :: IO FilePath
nextBackupDir = do
  now <- getCurrentTime
  return $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H-%M-%SZ")) now
    ++ yabaSuffix

ba :: IO AnchoredBackupTree
ba = do
  let backupDir = "./test/data/case3/backup"
  let sourceOfMainTree = "./test/data/case3/source-of-maintree"
  backup backupDir sourceOfMainTree
