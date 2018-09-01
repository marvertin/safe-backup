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
import           DirScan
import           Lib
import           Lodree
import           SliceScaner
import           SliceToLodree
import           System.Directory.Tree (AnchoredDirTree (..),
                                        DirTree (Dir, File), FileName)
import           System.FilePath
import           System.IO
import           TurboWare
import           Types


import qualified Data.ByteString.Lazy  as Lazy

readSourceTree :: FileName -> IO Lodree
readSourceTree dirName = do
    (base :/ d) <- readSlice dirName
    return$ currentLodree $ mergeToLodree emptyLodree d


scanDirectoryTest :: FilePath -> IO ()
-- scanDirectory = scanDirectory'' (\y b -> ()) (const True) (return . const ())
scanDirectoryTest path = do
   result <- scanDirectory (\rp list -> sum $ fmap snd list) (\rp -> True) readAndCountBytes path
   print "hotobo"
   where
     readAndCountBytes :: RevPath -> IO Integer
     readAndCountBytes revpath = do
       h <- openFile  (path </> (pth revpath)) ReadMode
       hSetBuffering h (BlockBuffering $ Just 100000)
       -- print (path </> (pth revpath))
       --dataa <- Lazy.readFile (path </> (pth revpath))
       --evaluate $ fromIntegral (Lazy.length dataa)
       dataa <- Lazy.hGetContents h
       evaluate $ fromIntegral (Lazy.length dataa)

readAndCountBytes0 :: RevPath -> IO Integer
readAndCountBytes0 x = return 0
