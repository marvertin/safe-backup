{-# LANGUAGE RecordWildCards #-}

module Yaba.IO.RestoreScriptWriter (
  createRestoreScripts
) where

import           Control.Monad
import           Data.Bifunctor
import           Data.List
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import           Data.Yaml
import           System.Directory

import           Text.Printf
import           Util.Lib
import           Util.TurboWare
import           Util.Types
import           Yaba.App.Context
import           Yaba.Data.Lodree
import           Yaba.IO.FileNamesC
import           Yaba.Process.RestoreScriptMaker

createRestoreScripts ::  Ctx -> Lodree -> IO Bool
createRestoreScripts ctx (LDir _ list) = foldr (createOnRestoreScript ctx) (return False) list

-- (M.Map FilePath UTCTime)
createOnRestoreScript :: Ctx -> (String, Lodree) -> IO Bool -> IO Bool
createOnRestoreScript Ctx{..} (sliceName, lodree) mustCreate = do

  let modificationTimesFilePath = takeSlicedDataPath sliceName ++ "/" ++ modificationTimesFileName
  -- print $ "ctu modifikance: " ++ modificationTimesFileName
  modificationTimes <- doesFileExist modificationTimesFilePath >>=
      \exist -> if exist  then (decodeFileThrow modificationTimesFilePath :: IO (M.Map FilePath UTCTime))
                          else return M.empty
  -- let modificationTimes = M.empty
  let scriptFileName = indexRoot ++ "/" ++ replaceVerticalToSlashes sliceName ++ "/restore.sh"
  mustCreate <- (||) <$> mustCreate <*> (not <$> doesFileExist scriptFileName)
  when (mustCreate) $ do
     writeFile scriptFileName (unlines $ makeRestoreScript modificationTimes lodree)
  return mustCreate
