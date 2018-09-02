module Backup (
  readBackupDir,
  backup
) where

import           BackupTreeBuilder
import           Control.Monad
import           Data.List
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Yaml
import           DirScan
import           Dump
import           Lib
import           Lodree
import           Slice
import           SliceScaner
import           SliceToLodree
import           SourceTree
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO
import           TurboWare
import           Types


isSliceName :: FileName -> Bool
isSliceName = isSuffixOf yabaSliceSuffix

readBackupDir :: EventHandler SliceTree b -> FilePath -> FilePath -> IO Lodree
readBackupDir eventHanlder backupRoot indexDir = do
  sliceNames <-  (map takeBaseName . sort . filter isSliceName) <$> listDirectory backupRoot
  putStrLn $ "Reading " ++ show (length sliceNames) ++ " slices allredy backed up"
  yabaDirs <- forM sliceNames (\name -> do
      slice <- readSlice'' eventHanlder (backupRoot </> name ++ yabaSliceSuffix)
      encodeFile (indexDir </> takeBaseName (fileNamex slice) ++ slicePhysicalTree_suffix) slice
      return slice
    )
  --forM_ yabaDirs (\x ->
    --  encodeFile (indexDir </> takeBaseName (fileNamex x) ++ slicePhysicalTree_suffix) x
    --)
  let rootLodree = mergesToLodree emptyLodree yabaDirs
  return rootLodree

--maintree = "maintree"

writeBackup :: Handle -> AnchoredBackupTree -> [(FileName, FilePath)] -> IO [AnchoredDirTree ()]
writeBackup loghandle x sourceTrees = do
    -- putStrLn $ "jsem v writeBackup"
    -- hFlush stdout
    -- nasledujici prikaz buh vi proc dlouho trva
    let (base :/ d@(Dir yabadir _)) = x
    putStrLn $ "Writing backup slice: " ++ yabadir
    hFlush stdout
    mapM ( \(treeName, treePath) -> do
       putStrLn $ "Writing tree: " ++ treePath ++ " ==> " ++ (yabadir </> treeName)
       putStrLn $ unlines $ dirTreeToStringList (Just . show) d
       -- musíme umazat adresář yaba a také adresář kořene
       let kolikSmazat = 1 + length yabadir + 1 + length treeName + length base
       writeDirectoryWith (writeFileToBackup kolikSmazat treePath) x
      ) sourceTrees
  where
    writeFileToBackup :: Int -> FilePath -> FilePath -> Cmd -> IO ()
    writeFileToBackup kolikSmazat sourceOfMainTreeDir path (Insert _) =
       let odkud = sourceOfMainTreeDir ++ drop kolikSmazat path
       in do
        hPutStrLn loghandle ("copy file: " ++ odkud ++ " --> " ++ path)
        copyFile odkud path
    writeFileToBackup _ _ path cmd = do
        let (dir, file) = splitFileName path
        let cesta = dir </> (yabaFilePrefix cmd ++ file) ++ ".yaba"
        hPutStrLn loghandle $ "create meta: " ++ cesta
            -- ++ unJabaContent (convertToJabaContent cmd)
        writeFile cesta (formatMetaFile . convertToYaba $ cmd)

yabaFilePrefix :: Cmd -> String
yabaFilePrefix (Insert _)                         = "~INSERT~"
yabaFilePrefix BackupTreeBuilder.Delete           = "~DELETE-OR-MOVE~"
yabaFilePrefix (BackupTreeBuilder.LogicalLink  _) = "~L-LINK~"
yabaFilePrefix (BackupTreeBuilder.PhysicalLink _) = "~P-LINK~"
yabaFilePrefix (NewLink _)                        = "~N-LINK~"

backup :: FilePath -> [(FileName, FilePath)] ->  IO [AnchoredDirTree ()]
backup backupDirRoot  sourceTrees = do
    createDirectoryIfMissing False dataDir
    createDirectoryIfMissing False indexDir
    createDirectoryIfMissing False logDir
    newSliceName <- nextSliceName
    withFile (logDir </> newSliceName ++ ".log") WriteMode (\handle -> do
      hSetBuffering handle LineBuffering
      let logger = hLoggingEventHandler handle
      let newSliceDirName = newSliceName ++ yabaSliceSuffix
      let newSlicePath = dataDir </> newSliceDirName
      lodreeBackupAll <- readBackupDir logger dataDir indexDir
      let lodreeBackupCurrent = currentLodree lodreeBackupAll
      putStrLn $  "Reading " ++ show (length sourceTrees) ++ " source trees"
      encodeFile (indexDir </> newSliceName ++ sliceLogicalTree_suffix) lodreeBackupCurrent
      encodeFile (indexDir </> sliceLogicalTree_suffix) lodreeBackupCurrent

      lodreeSourceAllNodes <- makeLDir <$> forM sourceTrees ( \(treeName, treePath) -> do
          lodreeSourceOneNode <- readSourceTree logger treePath
          encodeFile (indexDir </> (treeName ++ sliceSourceTree_suffix)) lodreeSourceOneNode
          return (treeName, lodreeSourceOneNode)
         )
      --lodreeSourceOneNode <- readSourceTree sourceOfMainTreeDir
      -- let lodreeSourceAllNodes = LDir emptyDRee [(maintree, lodreeSourceOneNode)]
      putStrLn $ "Building new backup slice: " ++ newSliceName
      case buildBackup lodreeBackupAll lodreeSourceAllNodes newSliceDirName of
        Nothing -> do
           putStrLn "NOTHING to backup: "
           return []
        Just backupDirTree -> do
           putStrLn $ "Writing backup to: " ++ dataDir
           writeBackup handle (dataDir :/ backupDirTree) sourceTrees
     )
  where
    dataDir = backupDirRoot </> dataSubdir
    indexDir = backupDirRoot </> indexSubdir
    logDir = backupDirRoot </> logSubdir
  -- return ()

nextSliceName :: IO String
nextSliceName = do
  now <- getCurrentTime
  return $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H-%M-%SZ")) now

convertToYaba :: Cmd -> SliceCmd
convertToYaba (BackupTreeBuilder.LogicalLink x)  = Slice.LogicalLink x
convertToYaba (BackupTreeBuilder.PhysicalLink x) = Slice.PhysicalLink x
convertToYaba (BackupTreeBuilder.NewLink x)      = Slice.PhysicalLink x
convertToYaba BackupTreeBuilder.Delete           = Slice.Delete


ba :: IO ()
ba = do
  let backupDir = "./test/data/case3/backup"
  let sourceOfMainTree = "./test/data/case3/source-of-maintree"
  backup backupDir [("maintree", sourceOfMainTree)]
  return ()
