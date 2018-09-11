{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module SourceTree (
 readSourceTree,
 scanDirectoryTest,
 CacheHash
) where

import qualified Crypto.Hash.SHA1      as Cr

import           Control.Exception
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.UTF8  as BSU
import           Data.Function
import           Data.List
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Time.Clock
import           System.Directory
import           System.Directory.Tree (AnchoredDirTree (..),
                                        DirTree (Dir, File), FileName)
import           System.FilePath
import           System.IO


import           DirScan
import           Ignorances
import           Lib
import           Lodree
import           Log
import           SliceScaner
import           SliceToLodree
import           TurboWare
import           Types


import qualified Data.ByteString.Lazy  as Lazy

type CacheHash = M.Map RevPath Ree

getEventHandler :: UTCTime -> Log -> (EventEnvelop a ErrList -> IO ErrList, ErrList)
getEventHandler time lo  = (logInScan time lo, ErrList [])

-- EventHandler Lodree b
readSourceTree :: Log -> CacheHash -> IgnoranceDef -> FilePath -> IO (Lodree, ErrList)
readSourceTree lo cacheHash ignorance rootDir = do
     startTime <- getCurrentTime
     scanDirectory (const makeLDir)
                     (makeFilterFce ignorance)
                     readLFile
                     (getEventHandler startTime lo)
                     rootDir
  where
    readLFile :: RevPath -> IO Lodree
    readLFile rp = do
        let path  = rootDir </> pth rp
        size <- getFileSize path
        time <- getModificationTime path
        flip LFile (pth rp) <$>
           case M.lookup rp cacheHash of
                 Nothing -> loadFileRee path
                 Just ree@Ree{..} -> if size == rsize && time == rtime
                                         then do
                                           putStrLn $ " zasaj kese: " ++ (pth rp)
                                           return ree
                                         else do
                                           putStrLn $ " NEKESA: " ++ (pth rp)
                                           loadFileRee path



scanDirectoryTest :: FilePath -> IO ()
-- scanDirectory = scanDirectory'' (\y b -> ()) (const True) (return . const ())
scanDirectoryTest path = do
   startTime <- getCurrentTime
   result <- scanDirectory (\rp list -> sum $ fmap snd list) (\rp -> True)
                readAndCountBytes
                (stdOutLoggingEventHanler startTime)
                path
   putStrLn "Hotovo"
   where
     readAndCountBytes :: RevPath -> IO Integer
     readAndCountBytes revpath = do
       h <- openFile  (path </> pth revpath) ReadMode
       hSetBuffering h (BlockBuffering $ Just 100000)
       -- print (path </> (pth revpath))
       --dataa <- Lazy.readFile (path </> (pth revpath))
       --evaluate $ fromIntegral (Lazy.length dataa)
       dataa <- Lazy.hGetContents h
       evaluate $ fromIntegral (Lazy.length dataa)
