{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Yaba.Command.MarkDuplicities (
  cmdMarkDuplicities
) where

import           Control.Monad
import qualified Data.Map            as M
import           Data.Time.Clock
import           System.Directory
import           System.Exit
import           System.FilePath


import           Util.DirScan
import           Util.Lib
import           Util.TurboWare
import           Util.Types
import           Yaba.App.Context
import           Yaba.App.Log
import           Yaba.Command.Backup
import           Yaba.Data.Lodree
import           Yaba.Data.Ree




getEventHandler :: UTCTime -> Log -> (EventEnvelop a ErrList -> IO ErrList, ErrList)
getEventHandler time lo  = (logInScan time lo, ErrList [])

cmdMarkDuplicities :: FilePath -> Ctx -> IO ExitCode
cmdMarkDuplicities markedDir ctx@Ctx{..} = do
    (slicinLodree, failusSlices) <- scanSlices ctx
    putStrLn "xxxxx0"
    let startTime = read "2018-09-17 20:28:57.1280774 UTC" :: UTCTime
    putStrLn $ "xxxxx1 " ++ show startTime
    (markedLodree, _) <- scanDirectory (const makeLDir)
                    (const True)
                    loadFile
                    (getEventHandler startTime lo)
                    markedDir
    putStrLn "xxxxx2"
    let hashesSlicin = createMapOfHashes slicinLodree
    let hashesMarked = createMapOfHashes markedLodree
    let beMarked = M.intersection hashesMarked hashesSlicin
    dump beMarked
    let list = concat (fst <$> M.elems beMarked )
    forM_ list (\p -> do
      let path1 = markedDir ++ p
      let path2 = takeDirectory path1 ++ "/~DUPLICITY~" ++ takeFileName path1
      putStrLn path1
      putStrLn path2
      putStrLn ""
      renameFile path1 path2

      )
    print list
    return ExitSuccess
  where
    loadFile :: RevPath -> IO Lodree
    loadFile rp = do
      ree <- loadFileRee (markedDir </> pth rp)
      return $ LFile ree ""
