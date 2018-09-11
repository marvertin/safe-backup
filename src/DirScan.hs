{-
   Scan direcotory and do some stuff for files and folders
-}
{-# LANGUAGE ScopedTypeVariables #-}

module DirScan (
  scanDirectory,
  pth,
  RevPath,
  emptyEventHandler,
  stdOutLoggingEventHanler,
  hLoggingEventHandler,
  EventHandler(..),
  Event(..),
  Cumulative(..),
  EventEnvelop(..),
  EventFile(..)
) where

import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Time.Clock
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Printf

import           Lib
import           Types

type FlowAvar = [(UTCTime, Int, Integer, UTCTime)] -- timce, count, size, header has latest
data Acum a b = Acum FlowAvar [(FilePath, a)] b deriving (Show)

data EventEnvelop a b = EventEnvelop RevPath Cumulative (Event a) b
    deriving (Show)

type EventHandler a b =   (EventEnvelop a b -> IO b, b)

data Event a
      = BeforeFile {
            efile :: EventFile
          }
      | AfterFile  {
            efile   :: EventFile,
            eresult :: a
          }
      | BeforeDir { efords :: [String] }
      | AfterDir  { efords     :: [String],
                    etuplelist :: [(FilePath, a)],
                    eresult    :: a
                  }
      | Start { erootPath :: FilePath }
      | End {
               erootPath :: FilePath,
               eresult   :: a
            }
      | Ignore
      | Failure IOException
    deriving (Show)


newtype EventFile = EventFile {
     efileSize :: Integer
   } deriving (Show)
data Cumulative = Cumulative {
      etotalCount       :: Int,
      etotalSize        :: Integer,
      avarageCountSpeed :: Double,
      avarageMbSpeed    :: Double
    } deriving (Show)

flowAvarEmpty :: UTCTime -> FlowAvar
flowAvarEmpty startTime = [(startTime, 0, 0, startTime), (startTime, 0, 0, startTime)]

emptyEventHandler :: EventHandler a ()
emptyEventHandler = (\x -> return (), ())

stdOutLoggingEventHanler = hLoggingEventHandler stdout
hLoggingEventHandler handle startTime = (printLog startTime handle, ())

scanDirectory :: Show a =>
        (RevPath -> [(FilePath, a)] -> a) -> -- directory node creator
        (RevPath -> Bool) ->                 -- dir or file filter
        (RevPath -> IO a) ->                -- file processor
        EventHandler a b ->
        FilePath ->                         -- scaned root
        IO (a, b)                           -- result,
scanDirectory createDirNode predicate createFileNode (eventFce, eventStart) rootPath = do
    startTime <- getCurrentTime
    let nula = 0 :: Int
    let startFlowAvar = flowAvarEmpty startTime
    evacum2 <- emitEvent' [] startFlowAvar (Start rootPath) eventStart
    Acum flowAvar ((_, result) : _) evacum3
      <- scanDirectory' 0 startTime (Acum startFlowAvar [] evacum2) []
    endTime <- getCurrentTime
    evacum4 <- emitEvent' [] [head flowAvar, last startFlowAvar]  (End rootPath result) evacum3
    --putStrLn $ "End scanning at " ++ show endTime ++ ", duration=" ++ show duration ++ "; total: "
    --printf "%6d# %10.3f MB | %9.2f #/s  %10.3f MB/s  \n" count (sizeInMb size) countSpeed sizeSpeed
    return (result, evacum4)
 where
  -- scanDirectory' :: Show a => Int -> Acum a -> RevPath -> IO (Acum a)
  scanDirectory' level startTime acum@(Acum flowAvar reslist evacum) revpath =
    if (not . null) revpath -- root level is not checked, predicate has never empty
        && (not . predicate) revpath then do -- if not skip whole subtree
       evacum2 <- emitEvent flowAvar Ignore evacum
       return $  Acum flowAvar reslist evacum2
    else do
      let fullPath = rootPath </> pth revpath
      -- putStrLn $ "Pokus: " ++ path
      isDir <- doesDirectoryExist fullPath
      catch (
        if isDir then do

           fords <- sort <$> listDirectory fullPath -- simple names
            -- fords <- fmap (fmap (path </>)) (listDirectory fullPath)
           evacum2 <- emitEvent flowAvar (BeforeDir fords) evacum
           Acum newFlowAvar lili evacum3 <- foldM (scanDirectory' (level + 1) startTime)
                                           (Acum flowAvar [] evacum2)
                                           (fmap (:revpath) (reverse fords)) -- foldM reverts it again
           let dirnode = createDirNode revpath lili
           evacum4 <- emitEvent flowAvar (AfterDir fords lili dirnode) evacum3
           return $  Acum newFlowAvar ((safeHead "" revpath, dirnode): reslist) evacum4

         else do
           sz <- getFileSize fullPath
           evacum2 <- emitEvent flowAvar (BeforeFile (EventFile sz)) evacum
           result <- createFileNode revpath -- can takes long
           nowTime <- getCurrentTime
           let newFlowAvar = updateFlowAvar flowAvar (1, fromIntegral sz) nowTime
           evacum3 <- emitEvent newFlowAvar (AfterFile (EventFile sz) result) evacum2
           let newAcum =  Acum newFlowAvar ((head revpath, result): reslist) evacum3
           return newAcum
        )  (\e  -> do
               let err = show (e :: IOException)
               evacum2 <- emitEvent flowAvar (Failure e) evacum
               return $  Acum flowAvar reslist evacum2
        )
    where emitEvent = emitEvent' revpath
  emitEvent'  revpath flowAvar event evacum =
     eventFce $ EventEnvelop revpath (getCumulative flowAvar) event evacum

  fullPth :: RevPath -> FilePath
  fullPth p = rootPath </> pth  p

-- | convert reverse path to forward path not starting with slash
-- | pth ["yaba", "home", "opt"] == opt/home/jaba
pth :: RevPath -> FilePath
pth = foldl (flip (</>)) []


printLog :: UTCTime -> Handle -> EventEnvelop a () -> IO ()
printLog startTime handle (EventEnvelop revpath (Cumulative count' size' countSpeed sizeSpeed) event _) = do
  case event of
    Ignore ->
      hPutStrLn handle $ "IGNORE: " ++ pth revpath
    Start rootPath -> do
      hPutStrLn handle "========================================================================="
      hPutStrLn handle $ "Start scanning: " ++ rootPath ++ " at " ++ show startTime
    End _ _ -> do
      endTime <- getCurrentTime
      hPutStrLn handle $ "End scanning at " ++ show endTime ++ ", duration=" ++ show (duration endTime) ++ "; total: "
      hPrintf handle "%6d# %10.3f MB | %9.2f #/s  %10.3f MB/s  \n" count' (sizeInMb size') countSpeed sizeSpeed
    AfterFile (EventFile size)  _ -> do
      time' <- getCurrentTime
      hPutStr handle $ duration time'
      hPrintf handle "%s %6d # %10.3f MB %9.2f #/s  %7.3f MB/s %10.3f %s\n"
          (take 19 $ show time') count' (sizeInMb size') countSpeed sizeSpeed (sizeInMb size) (pth revpath)
    BeforeFile (EventFile size) ->
      when (size > 1024 * 1024 * 100) (do
        hPrintf handle"  ... big file: %10.3f - %s \r" (sizeInMb size) (pth revpath)
        hFlush handle)
    Failure exc -> do
      let errstr = "!!!!! ERROR !!!!! " ++ show exc
      hPutStrLn handle errstr
      hPutStrLn stderr errstr
    _ -> return ()
  return ()
  where duration time' = take 6 (show (diffUTCTime time' startTime)) ++ "s "


updateFlowAvar :: FlowAvar -> (Int, Integer) -> UTCTime ->FlowAvar
updateFlowAvar flowavar (count', size') nowTime =
  let (lastTime, count1 , size1, latTraceTime )  = head flowavar
      count2 = count1 + count'
      size2 = size1 + size'
      jecas = diffUTCTime nowTime latTraceTime > 1.0
  in if jecas then (nowTime, count2, size2, nowTime) : take 10 flowavar
              else (nowTime, count2, size2, lastTime) : tail flowavar

getCumulative :: FlowAvar -> Cumulative
getCumulative flowAvar = let
     (countSpeed, sizeSpeed) = averageSpeed' (last flowAvar) (head flowAvar)
     (_, totalCount, totalSize, _) = head flowAvar
    in Cumulative totalCount totalSize countSpeed sizeSpeed



averageSpeed :: FlowAvar -> (Double, Double)
averageSpeed flowAvar = averageSpeed' (last flowAvar) (head flowAvar)

averageSpeed' :: (UTCTime, Int, Integer, UTCTime) -> (UTCTime, Int, Integer, UTCTime) -> (Double, Double)
averageSpeed' (time1, count1, size1, _) (time2, count2, size2, _) =
    let
        timeDiff :: Double = realToFrac  $ diffUTCTime time2 time1
    in (fromIntegral (count2 - count1) / timeDiff,
        fromIntegral (size2 - size1) / timeDiff / 1024 / 1024  )

safeHead :: a -> [a] -> a
safeHead def [] = def
safeHead _ l    = head l
