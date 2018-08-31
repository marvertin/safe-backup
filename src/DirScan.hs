
{-# LANGUAGE ScopedTypeVariables #-}

module DirScan (
  scanDirectory,
) where

import           BackupTreeBuilder
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy  as Lazy
import           Data.List
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Yaml
import           Dump
import           Lib
import           Lodree
import           Slice
import           SliceScaner
import           SliceToLodree
import           SourceTree
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO
import           System.IO             (hFlush, stdout)
import           Text.Printf
import           TurboWare
import           Types

type FlowAvar = [(UTCTime, Int, Integer)] -- timce, count, size, header has latest
data Acum a = Acum FlowAvar [a] deriving (Show)

type RevPath = [String] -- it is reverse list of path items: ["myfile.txt", "myaccount", "home", "opt"]

scanDirectory :: FilePath -> IO ()
-- scanDirectory = scanDirectory'' (\y b -> ()) (const True) (return . const ())
scanDirectory path = do
   result <- scanDirectory'' (\rp list -> sum $ fmap snd list) (\rp -> True) readAndCountBytes path
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



scanDirectory'' :: Show a =>
        (RevPath -> [(FileName, a)] -> a) -> -- directory node creator
        (RevPath -> Bool) ->                 -- dir or file filter
        (RevPath -> IO a) ->                -- file processor
        FilePath ->                         -- scaned root
        IO a                                -- result

scanDirectory'' createDirNode predicate createFileNode rootPath = do
    putStrLn ""
    startTime <- getCurrentTime
    Acum ((_, count, size): _) reslist <- scanDirectory' 0  (Acum [(startTime, 0, 0), (startTime, 0, 0)] []) []
    endTime <- getCurrentTime
    let (countSpeed, sizeSpeed) = averageSpeed' (startTime, 0, 0) (endTime, count, size)
    putStrLn "Total: "
    printf "%6d# %10.3f MB | %9.2f #/s  %10.3f MB/s  \n" count (sizeInMb size) countSpeed sizeSpeed
    putStrLn "-----------"
    print reslist
    putStrLn "-----------"
    return $ head reslist
 where
  -- scanDirectory' :: Show a => Int -> Acum a -> RevPath -> IO (Acum a)
  scanDirectory' level acum@(Acum flowAvar@((time, count, size) : _) reslist) revpath = do
    let fullPath = rootPath </> pth revpath
    -- putStrLn $ "Pokus: " ++ path
    isDir <- doesDirectoryExist fullPath
    -- time <- getCurrentTime
    --putStr $  "\r" ++ show level ++ ": " ++ show acum ++ "  " ++ replicate (level * 4) ' ' ++ path
    -- print time
    -- printf "%2d #%6d %10.3f MB %s %s  \n" level count (sizeInMb size) (show time) path
    if isDir then do
        fords <- listDirectory fullPath -- simple names
        -- fords <- fmap (fmap (path </>)) (listDirectory fullPath)
        Acum newFlowAvar lili <- foldM (scanDirectory' (level + 1)) (Acum flowAvar []) (fmap (:revpath) fords)
        let dirnode = createDirNode revpath (zip fords lili)
        return $  Acum newFlowAvar (dirnode: reslist)

     else do
       sz <- getFileSize fullPath
       result <- createFileNode revpath
       newFlowAvar <- updateFlowAvar flowAvar (count + 1, size + fromIntegral sz)
       let newAcum =  Acum newFlowAvar (result: reslist)
       return newAcum


  fullPth :: RevPath -> FilePath
  fullPth p = rootPath </> pth  p

pth :: RevPath -> FilePath
pth = foldl (flip (</>)) []





updateFlowAvar :: FlowAvar -> (Int, Integer) -> IO FlowAvar
updateFlowAvar flowavar (count, size) = do
  let (lastTime ,_ ,_ )  = head flowavar
  nowTime <- getCurrentTime
  let jecas = diffUTCTime nowTime lastTime > 1.0
  let newFlowAvar = if jecas then (nowTime, count, size) : take 10 flowavar
                             else (lastTime, count, size) : tail flowavar
  when jecas (
     let (countSpeed, sizeSpeed) = averageSpeed newFlowAvar
       in printf "%6d# %10.3f MB | %9.2f #/s  %10.3f MB/s  \n" count (sizeInMb size) countSpeed sizeSpeed
   )
  return newFlowAvar


averageSpeed :: FlowAvar -> (Double, Double)
averageSpeed flowAvar = averageSpeed' (last flowAvar) (head flowAvar)

averageSpeed' :: (UTCTime, Int, Integer) -> (UTCTime, Int, Integer) -> (Double, Double)
averageSpeed' (time1, count1, size1) (time2, count2, size2) =
    let
        timeDiff :: Double = realToFrac  $ diffUTCTime time2 time1
    in (fromIntegral (count2 - count1) / timeDiff,
        fromIntegral (size2 - size1) / timeDiff / 1024 / 1024  )

sizeInMb :: Integer -> Double
sizeInMb x =  fromIntegral x / 1024 / 1024
