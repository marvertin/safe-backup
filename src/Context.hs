{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Context (
  withContext,
  Ctx(..),

  ForestDef
) where

import           System.Directory
import           System.Exit
import           Text.Printf

import           Config
import           Lib
import           Log
import           SliceNameStrategy
import           TurboWare
import           Types
import           Yaba.IO.FileNamesC

data Ctx = Ctx {
     dataRoot            :: FilePath,
     indexRoot           :: FilePath,
     logRoot             :: FilePath,
     takeSlicedDataPath  :: String -> FilePath,
     takeSlicedIndexPath :: String -> FilePath,
     takeSlicedLogPath   :: String -> FilePath,
     sliceNameStrategy   :: SliceNameStrategy,
     newSliceName        :: String,
     forest              :: ForestDef, -- definition of forest prom config file
     empties             :: [String], -- trees withoud any file or directory after filtering
     lo                  :: Log
   }


withContext ::  FilePath -> (Ctx -> IO ExitCode) -> IO ExitCode
withContext backupDirRoot fce = do
  maybeForestDef <- readConfig backupDirRoot
  case maybeForestDef of
    Nothing -> do
      putStrLn "!!! ERRORS starting yaba !!!"
      return $ ExitFailure 1
    Just (Cfg sliceNameStrategy forest empties) -> do
      -- createDirectoryIfMissing False dataRoot
      let dataRoot = backupDirRoot ++ "/" ++ dataSubdir
      let indexRoot = backupDirRoot ++ "/" ++ indexSubdir
      let logRoot = backupDirRoot ++ "/" ++ logSubdir
      newSliceName <- nextSliceName dataRoot sliceNameStrategy
      --let slicedDirName dname = replaceVerticalToSlashes (dname ++ "/" ++ newSliceName)
      let takeSlicePath rotPath sliceName = rotPath ++ "/" ++ replaceVerticalToSlashes sliceName
      --let logDirx = slicedDirName logRoot
      --let indexDirx = slicedDirName indexRoot
      createDirectoryIfMissing True (takeSlicePath logRoot newSliceName)
      --createDirectoryIfMissing True indexDirx
      let logFileFath = takeSlicePath logRoot newSliceName ++ "/" ++ sliceLogName
      let yabaLogFilePath = logRoot ++ "/" ++ yabaLogName
      let ctx = Ctx {
           lo = \ _ _ -> return ()
         , dataRoot
         , indexRoot
         , logRoot
         , takeSlicedDataPath = takeSlicePath dataRoot
         , takeSlicedIndexPath = takeSlicePath indexRoot
         , takeSlicedLogPath = takeSlicePath logRoot
         , sliceNameStrategy
         , newSliceName
         , forest
         , empties
        }
      createRootDirs ctx
      withLogger yabaLogFilePath logFileFath $ \lo -> do
        lo Inf $ printf  "Detail log is: \"%s\"" logFileFath
        fce (ctx {lo})



createRootDirs :: Ctx -> IO ()
createRootDirs Ctx{..} = do
  createDirectoryIfMissing False dataRoot
  createDirectoryIfMissing False indexRoot
  createDirectoryIfMissing False logRoot
