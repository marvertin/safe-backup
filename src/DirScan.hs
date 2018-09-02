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
  EventHandler
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
data Acum a b = Acum FlowAvar [(FilePath, a)] b deriving (Show)

type RevPath = [String] -- it is reverse list of path items: ["myfile.txt", "myaccount", "home", "opt"]

data EventEnvelop a b = EventEnvelop RevPath Cumulative (Event a) b

type EventHandler a b =   (EventEnvelop a b -> IO b, IO b)

data Event a
      = BeforeFile {
            efile :: EventFile
          }
      | AfterFile  {
            efile   :: EventFile,
            eresult :: a
          }
      | BeforeDir [String]



data EventFile = EventFile {
     efileSize :: Integer
   }
data Cumulative = Cumulative {
      etotalCount       :: Int,
      etotalSize        :: Integer,
      avarageCountSpeed :: Double,
      avarageMbSpeed    :: Double
    }

flowAvarEmpty :: UTCTime -> FlowAvar
flowAvarEmpty startTime = [(startTime, 0, 0, startTime), (startTime, 0, 0, startTime)]

emptyEventHandler :: EventHandler a ()
emptyEventHandler = (\x -> return (), return ())

stdOutLoggingEventHanler = hLoggingEventHandler stdout
hLoggingEventHandler handle = (printLog handle, getCurrentTime)

scanDirectory :: Show a =>
        (RevPath -> [(FilePath, a)] -> a) -> -- directory node creator
        (RevPath -> Bool) ->                 -- dir or file filter
        (RevPath -> IO a) ->                -- file processor
        EventHandler a b ->
        FilePath ->                         -- scaned root
        IO a                                -- result
scanDirectory createDirNode predicate createFileNode (eventFce, eventStart) rootPath = do
    startTime <- getCurrentTime
    putStrLn $ "Start scanning: " ++ rootPath ++ " at " ++ show startTime
    let nula = 0 :: Int
    evStart <- eventStart
    Acum ((_, count, size, _): _) reslist _
      <- scanDirectory' 0 startTime (Acum (flowAvarEmpty startTime) [] evStart) []
    endTime <- getCurrentTime
    let (countSpeed, sizeSpeed) = averageSpeed' (startTime, 0, 0, startTime) (endTime, count, size, endTime)
    let duration = diffUTCTime endTime startTime
    putStrLn $ "End scanning at " ++ show endTime ++ ", duration=" ++ show duration ++ "; total: "
    printf "%6d# %10.3f MB | %9.2f #/s  %10.3f MB/s  \n" count (sizeInMb size) countSpeed sizeSpeed
    -- putStrLn $ "Result: " ++ show reslist
    return . snd . head $ reslist
 where
  -- scanDirectory' :: Show a => Int -> Acum a -> RevPath -> IO (Acum a)
  scanDirectory' level startTime acum@(Acum flowAvar reslist evacum) revpath = do
    if (not . null) revpath -- root level is not checked, predicate has never empty
        && (not . predicate) revpath then -- if not skip whole subtree
       return acum
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
         Acum newFlowAvar lili evacum2 <- foldM (scanDirectory' (level + 1) startTime)
                                         (Acum flowAvar [] evacum)
                                         (fmap (:revpath) (reverse fords)) -- foldM reverts it again
         let dirnode = createDirNode revpath lili
         return $  Acum newFlowAvar ((safeHead "" revpath, dirnode): reslist) evacum2

       else do
         sz <- getFileSize fullPath
         evacum2 <- emitEvent flowAvar (BeforeFile (EventFile sz)) evacum
         result <- createFileNode revpath -- can takes long
         nowTime <- getCurrentTime
         let newFlowAvar = updateFlowAvar flowAvar (1, fromIntegral sz) nowTime
         evacum3 <- emitEvent newFlowAvar (AfterFile (EventFile sz) result) evacum2
         let newAcum =  Acum newFlowAvar ((head revpath, result): reslist) evacum3
         return newAcum
        where
          emitEvent  flowAvar event evacum =
             eventFce $ EventEnvelop revpath
                             (getCumulative flowAvar)
                             event evacum


  fullPth :: RevPath -> FilePath
  fullPth p = rootPath </> pth  p

-- | convert reverse path to forward path not starting with slash
-- | pth ["yaba", "home", "opt"] == opt/home/jaba
pth :: RevPath -> FilePath
pth = foldl (flip (</>)) []


printLog :: Handle -> EventEnvelop a UTCTime -> IO UTCTime
printLog handle (EventEnvelop revpath (Cumulative count' size' countSpeed sizeSpeed) event startTime) =
  case event of
    AfterFile (EventFile size)  _ -> do
      time' <- getCurrentTime
      hPutStr handle $ take 6 (show (diffUTCTime time' startTime)) ++ "s "
      hPrintf handle "%s %6d # %10.3f MB %9.2f #/s  %7.3f MB/s %10.3f %s\n"
          (take 19 $ show time') count' (sizeInMb size') countSpeed sizeSpeed (sizeInMb size) (pth revpath)
      return startTime
    BeforeFile (EventFile size) -> do
      when (size > 1024 * 1024 * 100) (do
        hPrintf handle"  ... big file: %10.3f - %s \r" (sizeInMb size) (pth revpath)
        hFlush handle)
      return startTime

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

sizeInMb :: Integer -> Double
sizeInMb x =  fromIntegral x / 1024 / 1024

safeHead :: a -> [a] -> a
safeHead def [] = def
safeHead _ l    = head l
