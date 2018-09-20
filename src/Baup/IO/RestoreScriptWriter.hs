{-# LANGUAGE RecordWildCards #-}

module Baup.IO.RestoreScriptWriter (
  createRestoreScripts
) where

import           Control.Monad
import           Data.Bifunctor
import           Data.List
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import           Data.Yaml
import           System.Directory

import           Baup.App.Context
import           Baup.Data.Lodree
import           Baup.IO.FileNamesC
import           Baup.Process.RestoreScriptMaker
import           Text.Printf
import           Util.Lib
import           Util.TurboWare
import           Util.Types

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
  let scriptFileNameBash = indexRoot ++ "/" ++ replaceVerticalToSlashes sliceName ++ "/restore.sh"
  let scriptFileNameCmd = indexRoot ++ "/" ++ replaceVerticalToSlashes sliceName ++ "/restore.cmd"
  mustCreate <- (||) <$> mustCreate <*>
      ((||) <$> (not <$> doesFileExist scriptFileNameBash) <*> (not <$> doesFileExist scriptFileNameCmd))
  when (mustCreate) $ do
     writeFile scriptFileNameBash (unlines $ makeRestoreScript bashDefinition modificationTimes lodree)
     writeFile scriptFileNameCmd (unlines $ makeRestoreScript cmdDefinition modificationTimes lodree)
  return mustCreate
