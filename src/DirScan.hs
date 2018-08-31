
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
import           System.IO             (hFlush, stdout)
import           Text.Printf
import           TurboWare
import           Types

type FlowAvar = [(UTCTime, Int, Integer)] -- timce, count, size, header has latest
newtype Acum = Acum FlowAvar deriving (Show)

scanDirectory :: FilePath -> IO ()
scanDirectory path = do
    putStrLn ""
    startTime <- getCurrentTime
    Acum ((_, count, size): _) <- scanDirectory' 0  (Acum [(startTime, 0, 0), (startTime, 0, 0)]) path
    endTime <- getCurrentTime
    let (countSpeed, sizeSpeed) = averageSpeed' (startTime, 0, 0) (endTime, count, size)
    putStrLn "Total: "
    printf "%6d# %10.3f MB | %9.2f #/s  %10.3f MB/s  \n" count (sizeInMb size) countSpeed sizeSpeed
    putStrLn ""

scanDirectory' :: Int -> Acum -> FilePath -> IO Acum
scanDirectory' level acum@(Acum flowAvar@((time, count, size) : _)) path = do
  -- putStrLn $ "Pokus: " ++ path
  isDir <- doesDirectoryExist path
  -- time <- getCurrentTime
  --putStr $  "\r" ++ show level ++ ": " ++ show acum ++ "  " ++ replicate (level * 4) ' ' ++ path
  -- print time
  -- printf "%2d #%6d %10.3f MB %s %s  \n" level count (sizeInMb size) (show time) path
  if isDir then do
      fords <- fmap (fmap (path </>)) (listDirectory path)
      foldM (scanDirectory' (level + 1)) acum fords
   else do
     -- sz <- getFileSize path
     sz <- readAndCountBytes path
     Acum <$> updateFlowAvar flowAvar (count + 1, size + fromIntegral sz)


readAndCountBytes :: FilePath -> IO Integer
readAndCountBytes path = do
  dataa <- Lazy.readFile path
  evaluate $ fromIntegral (Lazy.length dataa)




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
