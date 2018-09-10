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
import           Data.Version          (showVersion)
import           Data.Yaml
import qualified Paths_yaba            (version)
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
import           SliceWriter
import           SourceTree
import           TreeComparator
import           TurboWare
import           Types

getEventHandler lo  = (logInScan lo, getCurrentTime)


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
          lo Summary $ "Start yaba " ++  showVersion Paths_yaba.version
          tmStart <- getCurrentTime
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
          lo Inf $ "    " ++ showRee (ree rootLodree)
          tmPhase1 <- getCurrentTime
          lo Summary $ printf "scaned %d slices - %s (%s)" (length sliceNames) (showRee (ree rootLodree)) (showDiffTm tmPhase1 tmStart)

          lo Inf "Phase 2/4 - reading source forest for backup"
          lo Inf $ printf "    %d trees in forest " (length forest)
          lodreeSourceAllNodes <- makeLDir <$> forM forest ( \(TreeDef treeName treePath ignorances) -> do
              lo Inf $ printf "       scaning %-15s- \"%s\"" treeName treePath
              lo Debug $ "ignorance patterns: " ++ (show ignorances)
              tmSourceStart <- getCurrentTime
              lodreeSourceOneNode <- readSourceTree lo ignorances treePath
              encodeFile (indexRoot </> (treeName ++ sliceSourceTree_suffix)) lodreeSourceOneNode
              tmSourceEnd <- getCurrentTime
              lo Summary $ printf "source %-15s-%s \"%s\" (%s)" treeName (showRee (ree lodreeSourceOneNode)) treePath (showDiffTm tmSourceEnd tmSourceStart)
              return (treeName, lodreeSourceOneNode)
             )
          lo Inf $ "    " ++ showRee (ree lodreeSourceAllNodes)
          tmPhase2 <- getCurrentTime
          lo Summary $ printf "forest of %d trees %s (%s)" (length forest) (showRee (ree lodreeSourceAllNodes)) (showDiffTm tmPhase2 tmPhase1)

          lo Inf "Phase 3/4 - comparing slices and source forest"
          let resulta = buildBackup rootLodree lodreeSourceAllNodes newSliceName

          tmPhase3 <- getCurrentTime
          case resulta of
            Nothing -> do
               lo Inf $ "Phase 4/4 - no differencies, NO backup: "
               lo Summary $ printf "********* NO BACKUP NEEDED ********** total time: %s" (showDiffTm tmPhase3 tmStart)
               return ExitSuccess
            Just (comareResult, backupDirTree) -> do
               lo Inf $ formatDiffResult comareResult
               forM_ (M.toList $ countCounters backupDirTree) (\(key, value) ->
                   forM_ [lo Inf, lo Summary] ($ printf "%8d: %s" value key)
                   )

               lo Inf $ "Phase 4/4 - copying files to new slice"
               lo Inf $ "    Writing new slice to: " ++ slicedDirName dataRoot
               (_  :/ resultOfCopy) <- writeBackup lo (dataRoot :/ backupDirTree) forest
               let failus = failures resultOfCopy
               tmPhase4 <- getCurrentTime
               if null failus
                 then do
                     if null empties then do
                        lo Inf $ "**** SUCCESS **** - backup has finished"
                        lo Summary $ printf "**** SUCCESS **** created new slice %s with  %d %s (%s)" newSliceName (countsToBackup backupDirTree) (showSz . sizeToBackup $ backupDirTree) (showDiffTm tmPhase4 tmPhase3)
                        return ExitSuccess
                       else do
                         lo Inf $  "**** success, BUT some trees are empty or unaccessible: " ++ (show empties)
                         lo Summary $ printf "created new slice %s with %d %s (%s) **** success, BUT some trees are empty or unaccessible: %s\n"
                            newSliceName (countsToBackup backupDirTree) (showSz . sizeToBackup $ backupDirTree) (showDiffTm tmPhase4 tmPhase3) (show empties)
                         return $ ExitFailure 7
                 else do
                     lo Error "!!!!!!!!!!!!!!!!!!! ERROR LIST !!!!!!!!!!!!"
                     forM_ failus (\oneFail -> do
                         -- putStrLn b
                           lo Error $ show oneFail
                       )
                     lo Error $ "!!!!!!!!!!!! " ++ show (length failus) ++ " ERRORS !!!!!!!!!"
                     return $ ExitFailure 2
        endTime <- getCurrentTime
        forM_ [lo Inf, lo Summary] ($ "Total time: " ++ showDiffTm endTime startTime ++ "\n")
        return exitCode
   where
    dataRoot = backupDirRoot </> dataSubdir
    indexRoot = backupDirRoot </> indexSubdir
    logRoot = backupDirRoot </> logSubdir
  -- return ()


formatDiffResult :: DirCompare -> String
formatDiffResult  compareResult =
  let (cl, sl, cr, sr) = diffCountAndSizes compareResult
  in printf "    deleted (%d #, %4.3f MB), inserted (%d #, %4.3f MB), diff (%d #, %4.3f MB)"
                  cl (sizeInMb sl) cr (sizeInMb sr) (cr - cl) (sizeInMb (sr - sl))
