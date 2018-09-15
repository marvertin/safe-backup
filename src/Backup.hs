{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module Backup (
  cmdBackup
) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Counter
import           Data.List
import qualified Data.Map                    as M
import           Data.Time.Clock
import           Data.Tuple
import           Data.Yaml
import           System.Directory
import           System.Directory.Tree
import           System.Exit
import           System.FilePath
import           System.IO
import           Text.Printf
import           Text.RawString.QQ

import           Config
import           Context
import           DirScan
import           Dump
import           Ignorances
import           Lib
import           Log
import           RestoreScript
import           SliceNameStrategy
import           TurboWare
import           Types
import           Yaba.Data.Differences
import           Yaba.Data.Lodree
import           Yaba.Data.Ree
import           Yaba.Data.Slicin
import           Yaba.Data.Slicout
import           Yaba.IO.FileNamesC
import           Yaba.IO.SlicinScaner
import           Yaba.IO.SlicoutWriter
import           Yaba.IO.SourceScaner
import           Yaba.Process.SlicinMerger
import           Yaba.Process.SlicoutBuilder
import           Yaba.Process.TreeComparator


getEventHandler :: UTCTime -> Log -> (EventEnvelop a ErrList -> IO ErrList, ErrList)
getEventHandler time lo  = (logInScan time lo, ErrList [])


cmdBackup :: Ctx -> IO ExitCode
cmdBackup ctx@Ctx{..} =  do -- gcc crashes whne versio is obtain from here
  startTime <- getCurrentTime
  exitCode <- do
    --lo Summary $ "Start yaba " ++  yabaVersion
    tmStart <- getCurrentTime

    (rootLodree, failusSlices) <- scanSlices ctx
    (lodreeSourceAllNodes, failusSurces) <- scanSources ctx
    resultOfCompare <- compareSlicesToSources ctx rootLodree lodreeSourceAllNodes
    failusCopy <- copyFiles ctx (snd <$> resultOfCompare)

    forM_ (failusSlices ++ failusSurces ++ failusCopy) (\msg -> do
        lo Summary $  msg
      )
    if null failusCopy && null failusSurces
         then do
           when (not . null $ empties) $ do
              forM_ [lo Inf, lo Summary] ($  "!!!! WARNING: some trees are empty: " ++ (show empties))
           lo Inf $ showSuccess
           return ExitSuccess
         else do
           when (not . null $ failusSlices) $
             lo Error $ printf  "!!!!! %d ERRORS while scaning slices !!!!!" (length failusSlices)
           when (not . null $ failusSurces) $
             lo Error $ printf  "!!!!! %d ERRORS while scaning sources !!!!!" (length failusSurces)
           when (not . null $ failusCopy) $
             lo Error $ printf  "!!!!! %d ERRORS while copiing files !!!!!" (length failusCopy)
           lo Error $ "!!!!! " ++ show (length failusCopy + length failusSurces + length failusSlices) ++ " ERRORS totally !!!!!"
           return $ ExitFailure 1

  endTime <- getCurrentTime
  forM_ [lo Inf, lo Summary] ($ "Total time: " ++ showDiffTm endTime startTime ++ "\n")
  return exitCode


scanSlices :: Ctx -> IO (Lodree, [String])
scanSlices ctx@Ctx{..} = do
  lo Inf "Phase 1/4 - reading slices backed up before"
  tmStart <- getCurrentTime
  sliceNames <-  listSlices  sliceNameStrategy dataRoot
  lo Inf $ if null sliceNames
              then "    no slices was backed up yet"
              else let decorate fce = (++"\"") . ("\""++) .  replaceVerticalToSlashes . fce
                   in printf "    %d slices: %s ... %s" (length sliceNames) (decorate head sliceNames) (decorate last sliceNames)
  forM_  (zip [1 :: Int ..] sliceNames) (\(n, slicen) -> lo Debug $ printf "%6d. %s" n slicen )
  (slices, failusSlices) <- fmap (>>= getErrList) . unzip <$>
      forM sliceNames (\name -> do
        let sliceIndexPath = (replaceVerticalToSlashes (indexRoot </> name </> sliceIndexName))
        let sliceIndexTempPath = (replaceVerticalToSlashes (indexRoot </> name </> "~~" ++ sliceIndexName))
        createDirectoryIfMissing True (takeDirectory sliceIndexPath)
        doesFileExist sliceIndexPath >>=
          (\exists -> if exists then do
                                   slice <- (decodeFileThrow sliceIndexPath :: IO Slicin)
                                   return (slice, ErrList [])
                                else do
                                   (slice, errs) <- readSlice (getEventHandler tmStart lo) (dataRoot </> name)
                                   when (null . getErrList $ errs)  $ do -- write index only whne there are no errors
                                     encodeFile sliceIndexTempPath slice
                                     slice2 <- (decodeFileThrow sliceIndexTempPath :: IO Slicin)
                                     if (slice == slice2)
                                        then do
                                               renameFile sliceIndexTempPath sliceIndexPath
                                               lo Inf $ "    created index: \"" ++ sliceIndexPath ++ "\""
                                        else
                                               lo Error $ "IMPOSSIBLE: written a read slices are not same!"
                                   return (slice, errs)
             )
    )
  let rootLodree = mergesToLodree emptyLodree slices
  let lodreeBackupCurrent = currentLodree rootLodree
  -- M.Map FilePath UTCTime

  anyScriptCreated <- createRestoreScripts ctx rootLodree
  when (anyScriptCreated) $ lo Inf "    Any restore script has been created."
  -- createDirectoryIfMissing True (takeSlicedIndexPath newSliceName)
  encodeFile (takeSlicedLogPath newSliceName </> sliceLogicalTree_suffix) lodreeBackupCurrent

  lo Inf $ "    " ++ showRee (ree rootLodree)
  tmEnd <- getCurrentTime
  lo Inf $ showPhaseTime tmEnd tmStart
  lo Summary $ printf "scaned %d slices - %s (%s)" (length sliceNames) (showRee (ree rootLodree)) (showDiffTm tmEnd tmStart)
  return (rootLodree, failusSlices)
  -- return ()


scanSources :: Ctx -> IO (Lodree, [String])
scanSources ctx@Ctx{..} = do
  lo Inf "Phase 2/4 - reading source forest for backup"
  tmStart <- getCurrentTime
  lo Inf $ printf "    %d trees in forest " (length forest)
  (lodreeSourceAllNodes, failusSurces) <- bimap makeLDir (>>= getErrList) . unzip <$>
       forM forest ( \(TreeDef treeName treePath ignorances) -> do
          lo Inf $ printf "       scaning %-15s- \"%s\"" treeName treePath
          lo Debug $ "ignorance patterns: " ++ (show ignorances)
          tmSourceStart <- getCurrentTime
          let cacheHashPath = (indexRoot </> (indexVersion ++ "_" ++ treeName ++ ".yaml"))
          cacheHash <- doesFileExist cacheHashPath >>=
                             (\exists -> if exists then (decodeFileThrow cacheHashPath :: IO CacheHash)
                                                   else return M.empty)
          (lodreeSourceOneNode, errList) <- readSourceTree lo cacheHash ignorances treePath
          -- encodeFile (indexRoot </> (treeName ++ sliceSourceTree_suffix)) lodreeSourceOneNode
          -- encodeFile cacheHashPath (flattenFileLodree lodreeSourceOneNode)
          encodeFile cacheHashPath (M.fromList (flattenFileLodree lodreeSourceOneNode))
          tmSourceEnd <- getCurrentTime
          lo Summary $ printf "source %-15s-%s \"%s\" (%s)" treeName (showRee (ree lodreeSourceOneNode)) treePath (showDiffTm tmSourceEnd tmSourceStart)
          return ((treeName, lodreeSourceOneNode), errList)
     )
  lo Inf $ "    " ++ showRee (ree lodreeSourceAllNodes)
  tmEnd <- getCurrentTime
  lo Inf $ showPhaseTime tmEnd tmStart
  lo Summary $ printf "forest of %d trees %s (%s)" (length forest) (showRee (ree lodreeSourceAllNodes)) (showDiffTm tmEnd tmStart)
  return (lodreeSourceAllNodes, failusSurces)

compareSlicesToSources :: Ctx -> Lodree -> Lodree -> IO (Maybe (Differences, Slicout))
compareSlicesToSources ctx@Ctx{..} rootLodree lodreeSourceAllNodes = do
  lo Inf "Phase 3/4 - comparing slices and source forest"
  tmStart <- getCurrentTime
  let resulta = buildBackup rootLodree lodreeSourceAllNodes newSliceName
  case resulta of
    Nothing -> do
       lo Inf $ "    no differences"
    Just (comareResult, backupDirTree) -> do
       forM_ [lo Inf, lo Summary] ($ formatDiffResult comareResult)
       forM_ (M.toList $ countCounters backupDirTree) (\(key, value) ->
           forM_ [lo Inf, lo Summary] ($ printf "%8d: %s" value key)
           )
  tmEnd <- getCurrentTime
  lo Inf $ showPhaseTime tmEnd tmStart
  return resulta

copyFiles :: Ctx -> Maybe Slicout -> IO [String]
copyFiles ctx@Ctx{..} resulta = do
  lo Inf $ "Phase 4/4 - copying files to new slice"
  tmStart <- getCurrentTime
  case resulta of
    Nothing -> do
       lo Inf $ "    skipped, no differencies, NO BACKUP NEEDED: "
       return []
    Just backupDirTree -> do
       lo Inf $ "    Writing new slice to: " ++ takeSlicedDataPath newSliceName
       createDirectoryIfMissing True (takeSlicedDataPath newSliceName)
       encodeFile (takeSlicedDataPath newSliceName ++ "/" ++ modificationTimesFileName)  (modificationTimes backupDirTree)
       (_  :/ resultOfCopy) <- writeBackup lo (dataRoot :/ backupDirTree) forest
       let failus :: [DirTree (Int, Integer, Int)]
           failus = failures resultOfCopy
       let failusStr = (show . err) <$> failus
       forM_ failusStr (\msg -> do
           lo Error $ "    !!!!! ERROR !!!!! " ++ msg
         )
       let MonoidPlus3 (copiedFiles, copiedSize, createdMetas) = foldMap MonoidPlus3  resultOfCopy
       let msg = printf "copied %d files of %s, created %d metafiles into \"%s\"" copiedFiles (showSz copiedSize) createdMetas (replaceVerticalToSlashes newSliceName) :: String
       lo Inf $ printf "    %s" msg

       tmEnd <- getCurrentTime
       lo Summary $ printf "%s (%s)" msg (showDiffTm tmEnd tmStart)
       lo Inf $ showPhaseTime tmEnd tmStart
       return failusStr

showPhaseTime tmEnd tmStart  = "    (" ++ showDiffTm tmEnd tmStart ++ ")"

showSuccess = [r|---------------------------------------------------------
*** OK *** OK *** OK *** SUCCESS *** OK *** OK *** OK ***
|]

formatDiffResult :: Differences -> String
formatDiffResult  compareResult =
  let ((cl, sl), (cr, sr)) = diffCountAndSizes compareResult
  in printf "    deleted (%d #, %4.3f MB), inserted (%d #, %4.3f MB), diff (%d #, %4.3f MB)"
                  cl (sizeInMb sl) cr (sizeInMb sr) (cr - cl) (sizeInMb (sr - sl))
