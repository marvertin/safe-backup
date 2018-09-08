{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Backup (
  backup
) where

import           Control.Monad
import           Data.Counter
import           Data.List
import qualified Data.Map              as M
import           Data.Time.Clock
import           Data.Yaml
import           System.Directory
import           System.Directory.Tree
import           System.Exit
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
import           Log
import           Slice
import           SliceNameStrategy
import           SliceScaner
import           SliceToLodree
import           SourceTree
import           TurboWare
import           Types

getEventHandler lo  = (logInScan lo, getCurrentTime)


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
    writeFileToBackup kolikSmazat sourceOfMainTreeDir path Insert  =
       let odkud = sourceOfMainTreeDir ++ drop kolikSmazat path
       in do
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

backup :: FilePath -> IO ExitCode
backup backupDirRoot = do
  maybeForestDef <- readConfig backupDirRoot
  case maybeForestDef of
    Nothing -> do
      putStrLn "!!! ERRORS starting backup !!!"
      return $ ExitFailure 1
    Just (Cfg sliceNameStrategy forest empties) -> do
      createDirectoryIfMissing False dataRoot
      newSliceName <- nextSliceName dataRoot sliceNameStrategy
      let slicedDirName dname = replaceVerticalToSlashes (dname </> newSliceName)
      let logDirx = slicedDirName logRoot
      let indexDirx = slicedDirName indexRoot
      createDirectoryIfMissing True logDirx
      createDirectoryIfMissing True indexDirx
      let logFileName = logDirx </> sliceLogName
      let yabaLogFilePath = logRoot </> yabaLogName
      withLogger yabaLogFilePath logFileName $ \lo -> do
        startTime <- getCurrentTime
        exitCode <- do
          lo Inf $ printf  "Detail log is: \"%s\"" logFileName
          lo Inf "Phase 1/4 - reading slices backed up before"
          sliceNames <-  listSlices  sliceNameStrategy dataRoot
          lo Inf $ if null sliceNames
                      then "    no slices was backed up yet"
                      else printf "    %d slices: %s ... %s" (length sliceNames) (head sliceNames) (last sliceNames)
          forM_  (zip [1 :: Int ..] sliceNames) (\(n, slicen) -> lo Debug $ printf "%6d. %s" n slicen )
          slices <- forM sliceNames (\name -> do
              slice <- readSlice (getEventHandler lo) (dataRoot </> name)
              encodeFile (replaceVerticalToSlashes (indexDirx </> slicePhysicalTree_suffix)) slice
              return slice
            )
          let rootLodree = mergesToLodree emptyLodree slices
          let lodreeBackupCurrent = currentLodree rootLodree
          encodeFile (indexDirx </> sliceLogicalTree_suffix) lodreeBackupCurrent

          lo Inf "Phase 2/4 - reading source forest for backup"
          lo Inf $ printf "    %d trees in forest " (length forest)
          lodreeSourceAllNodes <- makeLDir <$> forM forest ( \(TreeDef treeName treePath ignorances) -> do
              lo Inf $ printf "    scaning %-15s- \"%s\"" treeName treePath
              lo Debug $ "ignorance patterns: " ++ (show ignorances)
              lodreeSourceOneNode <- readSourceTree lo ignorances treePath
              encodeFile (indexRoot </> (treeName ++ sliceSourceTree_suffix)) lodreeSourceOneNode
              return (treeName, lodreeSourceOneNode)
             )
          --lodreeSourceOneNode <- readSourceTree sourceOfMainTreeDir
          -- let lodreeSourceAllNodes = LDir emptyDRee [(maintree, lodreeSourceOneNode)]
          lo Inf $ "Phase 3/4 - comparing slices and source forest"
          let resulta = buildBackup rootLodree lodreeSourceAllNodes newSliceName

          case resulta of
            Nothing -> do
               lo Inf $ "Phase 4/4 - no differencies, NO backup: "
               return ExitSuccess
            Just backupDirTree -> do
               forM_ (M.toList $ countCounters backupDirTree) (\(key, value) ->
                   lo Inf $ printf "%8d: %s" value key
                   )

               lo Inf $ "Phase 4/4 - copying files to new slice"
               lo Inf $ "    Writing new slice to: " ++ slicedDirName dataRoot
               results <- writeBackup lo (dataRoot :/ backupDirTree) forest
               let failus = fmap (\(b :/ d) -> (b, failures d)) results
               let failus2 = failus >>= \(b, list)  -> (b,) <$> list
               if null failus2
                 then do
                     if null empties then do
                        lo Inf $ "**** SUCCESS **** - backup has finished"
                        return ExitSuccess
                       else do
                         lo Inf $  "**** success, BUT some trees are empty or unaccessible: " ++ (show empties)
                         return $ ExitFailure 7
                 else do
                     lo Error "!!!!!!!!!!!!!!!!!!! ERROR LIST !!!!!!!!!!!!"
                     forM_ failus (\(b, list) -> do
                         putStrLn b
                         forM_ list (\x -> do
                           putStr "    "
                           print x
                          )
                       )
                     lo Error $ "!!!!!!!!!!!! " ++ show (length failus2) ++ " ERRORS !!!!!!!!!"
                     return $ ExitFailure 2
        endTime <- getCurrentTime
        lo Summary  $ "Total time: " ++ show (diffUTCTime endTime startTime)
        return exitCode
   where
    dataRoot = backupDirRoot </> dataSubdir
    indexRoot = backupDirRoot </> indexSubdir
    logRoot = backupDirRoot </> logSubdir
  -- return ()


convertToSliceCmd :: Cmd -> SliceCmd
convertToSliceCmd (BackupTreeBuilder.Link  x _) = Slice.PhysicalLink x
convertToSliceCmd (BackupTreeBuilder.Delete _ ) = Slice.Delete
