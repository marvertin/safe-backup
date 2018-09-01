{-
   Scan direcotory and do some stuff for files and folders
-}
{-# LANGUAGE ScopedTypeVariables #-}

module DirScan (
  scanDirectory,
  pth,
  RevPath
) where

import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Time.Clock
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Printf

type FlowAvar = [(UTCTime, Int, Integer, UTCTime)] -- timce, count, size, header has latest
data Acum a = Acum FlowAvar [a] deriving (Show)

type RevPath = [String] -- it is reverse list of path items: ["myfile.txt", "myaccount", "home", "opt"]

flowAvarEmpty :: UTCTime -> FlowAvar
flowAvarEmpty startTime = [(startTime, 0, 0, startTime), (startTime, 0, 0, startTime)]



scanDirectory :: Show a =>
        (RevPath -> [(FilePath, a)] -> a) -> -- directory node creator
        (RevPath -> Bool) ->                 -- dir or file filter
        (RevPath -> IO a) ->                -- file processor
        FilePath ->                         -- scaned root
        IO a                                -- result
scanDirectory createDirNode predicate createFileNode rootPath = do
    startTime <- getCurrentTime
    putStrLn $ "Start scanning: " ++ rootPath ++ " at " ++ show startTime
    Acum ((_, count, size, _): _) reslist <- scanDirectory' 0 startTime (Acum (flowAvarEmpty startTime)  []) []
    endTime <- getCurrentTime
    let (countSpeed, sizeSpeed) = averageSpeed' (startTime, 0, 0, startTime) (endTime, count, size, endTime)
    let duration = diffUTCTime endTime startTime
    putStrLn $ "End scanning at " ++ show endTime ++ ", duration=" ++ show duration ++ "; total: "
    printf "%6d# %10.3f MB | %9.2f #/s  %10.3f MB/s  \n" count (sizeInMb size) countSpeed sizeSpeed
    putStrLn $ "Result: " ++ show reslist
    return $ head reslist
 where
  -- scanDirectory' :: Show a => Int -> Acum a -> RevPath -> IO (Acum a)
  scanDirectory' level startTime acum@(Acum flowAvar reslist) revpath = do
    if False && (not $ predicate revpath) then return acum
    else do
      let fullPath = rootPath </> pth revpath
      -- putStrLn $ "Pokus: " ++ path
      isDir <- doesDirectoryExist fullPath
      -- time <- getCurrentTime
      --putStr $  "\r" ++ show level ++ ": " ++ show acum ++ "  " ++ replicate (level * 4) ' ' ++ path
      -- print time
      -- printf "%2d #%6d %10.3f MB %s %s  \n" level count (sizeInMb size) (show time) path
      if isDir then do
          fords <- sort <$> listDirectory fullPath -- simple names
          -- fords <- fmap (fmap (path </>)) (listDirectory fullPath)
          Acum newFlowAvar lili <- foldM (scanDirectory' (level + 1) startTime)
                                         (Acum flowAvar [])
                                         (fmap (:revpath) (reverse fords)) -- foldM reverts it again
          let dirnode = createDirNode revpath (zip fords lili)
          return $  Acum newFlowAvar (dirnode: reslist)

       else do
         sz <- getFileSize fullPath
         when (sz > 1024 * 1024 * 100) (do
           printf "  ... big file: %10.3f - %s \r" (sizeInMb sz) fullPath
           hFlush stdout)
         result <- createFileNode revpath -- can takes long
         nowTime <- getCurrentTime
         let newFlowAvar = updateFlowAvar flowAvar (1, fromIntegral sz) nowTime
         putStr $ take 6 (show (diffUTCTime nowTime startTime)) ++ "s "
         printPostup newFlowAvar (sz, revpath)
         let newAcum =  Acum newFlowAvar (result: reslist)
         return newAcum


  fullPth :: RevPath -> FilePath
  fullPth p = rootPath </> pth  p

-- | convert reverse path to forward path not starting with slash
-- | pth ["yaba", "home", "opt"] == opt/home/jaba
pth :: RevPath -> FilePath
pth = foldl (flip (</>)) []


printPostup :: FlowAvar -> (Integer, RevPath) -> IO ()
printPostup flowAvar@((time', count', size', _):_) (size, revpath) = do
    let (countSpeed, sizeSpeed) = averageSpeed flowAvar
    printf "%s %6d # %10.3f MB %9.2f #/s  %7.3f MB/s %10.3f %s\n"
          (take 19 $ show time') count' (sizeInMb size') countSpeed sizeSpeed (sizeInMb size) (pth revpath)

updateFlowAvar :: FlowAvar -> (Int, Integer) -> UTCTime ->FlowAvar
updateFlowAvar flowavar (count', size') nowTime =
  let (lastTime, count1 , size1, latTraceTime )  = head flowavar
      count2 = count1 + count'
      size2 = size1 + size'
      jecas = diffUTCTime nowTime latTraceTime > 1.0
  in if jecas then (nowTime, count2, size2, nowTime) : take 10 flowavar
              else (nowTime, count2, size2, lastTime) : tail flowavar



averageSpeed :: FlowAvar -> (Double, Double)
averageSpeed flowAvar = averageSpeed' (last flowAvar) (head flowAvar)

averageSpeed' :: (UTCTime, Int, Integer, UTCTime) -> (UTCTime, Int, Integer, UTCTime) -> (Double, Double)
averageSpeed' (time1, count1, size1, _) (time2, count2, size2, _) =
    let
        timeDiff :: Double = realToFrac  $ diffUTCTime time2 time1
    in (fromIntegral (count2 - count1) / timeDiff,
        fromIntegral (size2 - size1) / timeDiff / 1024 / 1024  )

sizeInMb :: Integer -> Double
sizeInMb x =  fromIntegral x / 1024 / 1024
