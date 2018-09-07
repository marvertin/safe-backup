{-# LANGUAGE RecordWildCards #-}

module Backup (
  readBackupDir,
  backup
) where

import           Control.Monad
import           Data.Counter
import           Data.List
import qualified Data.Map              as M
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Yaml
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO
import           Text.Printf

import           BackupTreeBuilder
import           Config
import           DirScan
import           Dump
import           Ignorances
import           Lib
import           Lodree
import           Slice
import           SliceScaner
import           SliceToLodree
import           SourceTree
import           TurboWare
import           Types


readBackupDir :: EventHandler SliceTree b -> FilePath -> FilePath -> IO Lodree
readBackupDir eventHanlder backupRoot indexDir = do
  -- TODO strategie
  sliceNames <-  sort <$> listDirectory backupRoot
  putStrLn $ "Reading " ++ show (length sliceNames) ++ " slices allredy backed up"
  yabaDirs <- forM sliceNames (\name -> do
      slice <- readSlice'' eventHanlder (backupRoot </> name)
      encodeFile (indexDir </> takeBaseName (fileNamex slice) ++ slicePhysicalTree_suffix) slice
      return slice
    )
  --forM_ yabaDirs (\x ->
    --  encodeFile (indexDir </> takeBaseName (fileNamex x) ++ slicePhysicalTree_suffix) x
    --)
  let rootLodree = mergesToLodree emptyLodree yabaDirs
  return rootLodree

--maintree = "maintree"

writeBackup :: Handle -> AnchoredBackupTree -> ForestDef -> IO [AnchoredDirTree ()]
writeBackup loghandle x sourceTrees = do
    -- putStrLn $ "jsem v writeBackup"
    -- hFlush stdout
    -- nasledujici prikaz buh vi proc dlouho trva
    let (base :/ d@(Dir yabadir _)) = x
    putStrLn $ "Writing backup slice: " ++ yabadir
    forM_ (M.toList $ countCounters d) (\(key, value) -> do
         printf "%8d: %s\n" value key
         )
    hFlush stdout
    mapM ( \(TreeDef treeName treePath _) -> do
       --putStrLn $ "Writing tree: " ++ treePath ++ " ==> " ++ (yabadir </> treeName)
       --putStrLn $ unlines $ dirTreeToStringList (Just . show) d
       -- musíme umazat adresář yaba a také adresář kořene
       let kolikSmazat = 1 + length yabadir + 1 + length treeName + length base
       writeDirectoryWith (writeFileToBackup kolikSmazat treePath) x
      ) sourceTrees
  where
    writeFileToBackup :: Int -> FilePath -> FilePath -> Cmd -> IO ()
    writeFileToBackup kolikSmazat sourceOfMainTreeDir path Insert  =
       let odkud = sourceOfMainTreeDir ++ drop kolikSmazat path
       in do
        hPutStrLn loghandle ("copy file: " ++ odkud ++ " --> " ++ path)
        copyFile odkud path
    writeFileToBackup _ _ path cmd = do
        let (dir, file) = splitFileName path
        let cesta = dir </> (yabaFilePrefix cmd ++ file) ++ ".yaba"
        hPutStrLn loghandle $ "create meta: " ++ cesta
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

backup :: FilePath -> ForestDef -> IO [AnchoredDirTree ()]
backup backupDirRoot  sourceTrees  = do
    createDirectoryIfMissing False dataDir
    createDirectoryIfMissing False indexDir
    createDirectoryIfMissing False logDir
    newSliceName <- nextSliceName
    withFile (logDir </> newSliceName ++ ".log") WriteMode (\handle -> do
      hSetBuffering handle LineBuffering
      let logger = hLoggingEventHandler handle
      -- TODO strategie
      let newSliceDirName = newSliceName
      let newSlicePath = dataDir </> newSliceDirName
      lodreeBackupAll <- readBackupDir logger dataDir indexDir
      let lodreeBackupCurrent = currentLodree lodreeBackupAll
      putStrLn $  "Reading " ++ show (length sourceTrees) ++ " source trees"
      encodeFile (indexDir </> newSliceName ++ sliceLogicalTree_suffix) lodreeBackupCurrent
      encodeFile (indexDir </> sliceLogicalTree_suffix) lodreeBackupCurrent

      lodreeSourceAllNodes <- makeLDir <$> forM sourceTrees ( \(TreeDef treeName treePath ignorances) -> do
          lodreeSourceOneNode <- readSourceTree logger ignorances treePath
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

convertToSliceCmd :: Cmd -> SliceCmd
convertToSliceCmd (BackupTreeBuilder.Link  x _) = Slice.PhysicalLink x
convertToSliceCmd (BackupTreeBuilder.Delete _ ) = Slice.Delete
