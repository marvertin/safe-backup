module Log (
  withLogger,
  Level(..),
  Log,
  logInScan
  -- lo
) where

import           Control.Monad
import           Data.Time.Clock
import           System.IO
import           Text.Printf

import           DirScan
import           Lib

type Log = Level -> String -> IO ()

data Level
  = Debug -- only to log file
  | Inf -- to stdout and to log file
  | Progress -- only to stdout only if terminal is attached, not scroll "\rtext\t"
  | Summary -- only to sumary file
  | Error -- to stderr and to log file
  deriving (Show)

withLogger :: FilePath ->  FilePath -> (Log -> IO a) -> IO a
withLogger yabaLogPath sliceLogPath fce =
  withFile yabaLogPath AppendMode (\yhandle ->
    withFile sliceLogPath WriteMode (\shandle ->
      fce (doLog yhandle shandle)
     )
   )

-- fmap ((show l ++ ": "):) .
doLog :: Handle -> Handle -> Level -> String -> IO()
doLog yh sh l s = do
    let ss = show l ++ ": " ++ s
    case l of
      Debug -> hPutStrLn sh $ ss
      Summary -> do
        time <- getCurrentTime
        hPutStrLn sh $ ss
        hPutStrLn yh $ (take 19 . show $ time) ++ ": " ++ s
      Error -> do
        hPutStrLn stderr $ ss
        hPutStrLn sh $ ss
      _ -> do
        putStrLn $ ss
        hPutStrLn sh $ ss

loa :: String -> IO ()
loa xx = do
   bufan <- hGetBuffering stdout
   let kon =  LineBuffering == bufan
   putStr $ show kon ++ ": "
   putStrLn xx
   hPutStrLn stderr xx

logInScan :: Log -> EventEnvelop a UTCTime -> IO UTCTime
logInScan lo (EventEnvelop revpath (Cumulative count' size' countSpeed sizeSpeed) event startTime) = do
 case event of
   Ignore ->
     lo Debug $ "IGNORE: " ++ pth revpath
   Start rootPath -> do
     lo Debug $  "========================================================================="
     lo Debug $ "Start scanning: " ++ rootPath ++ " at " ++ show startTime
   End _ _ -> do
     endTime <- getCurrentTime
     lo Debug $ "End scanning at " ++ show endTime ++ ", duration=" ++ show (duration endTime) ++ "; total: "
     -- not LN
     lo Debug $ printf "%6d# %10.3f MB | %9.2f #/s  %10.3f MB/s  " count' (sizeInMb size') countSpeed sizeSpeed
   AfterFile (EventFile size)  _ -> do
     time' <- getCurrentTime
     -- lo Debug $  duration time'
     lo Debug $ printf "%s %6d # %10.3f MB %9.2f #/s  %7.3f MB/s %10.3f %s"
         (take 19 $ show time') count' (sizeInMb size') countSpeed sizeSpeed (sizeInMb size) (pth revpath)
   BeforeFile (EventFile size) ->
     when (size > 1024 * 1024 * 100) (do
       lo Debug $ printf "  ... big file: %10.3f - %s" (sizeInMb size) (pth revpath)
       )
   Failure exc -> do
     let errstr = "!!!!! ERROR !!!!! " ++ show exc
     lo Error $ errstr
   _ -> return ()
 return startTime
 where duration time' = take 6 (show (diffUTCTime time' startTime)) ++ "s "
