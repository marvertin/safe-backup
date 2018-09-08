{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module SourceTree (
 readSourceTree,
 scanDirectoryTest
) where

import qualified Crypto.Hash.SHA1      as Cr

import           Control.Exception
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.UTF8  as BSU
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Time.Clock
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

getEventHandler lo  = (logInScan lo, getCurrentTime)

-- EventHandler Lodree b
readSourceTree :: Log -> IgnoranceDef -> FilePath -> IO Lodree
readSourceTree lo ignorance rootDir =
     fst <$> scanDirectory (const makeLDir)
                     (makeFilterFce ignorance)
                     readLFile
                     (getEventHandler lo)
                     rootDir
  where
    readLFile :: RevPath -> IO Lodree
    readLFile rp =  (flip LFile) (pth rp) <$> loadFileRee (rootDir </> pth rp)


scanDirectoryTest :: FilePath -> IO ()
-- scanDirectory = scanDirectory'' (\y b -> ()) (const True) (return . const ())
scanDirectoryTest path = do
   result <- scanDirectory (\rp list -> sum $ fmap snd list) (\rp -> True)
                readAndCountBytes
                stdOutLoggingEventHanler
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
