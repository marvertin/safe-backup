{-# LANGUAGE NamedFieldPuns #-}
module Yaba.App.Log (
  withLogger,
  Level(..),
  Log,
  logInScan
  -- lo
) where

import           Control.Monad
import           Data.IORef
import           Data.Time.Clock
import qualified System.Console.Terminal.Size as Terminal
import           System.IO
import           Text.Printf


import           Util.DirScan
import           Util.Lib
import           Util.Types

type Log = Level -> String -> IO ()

data Level
  = Debug -- only to log file
  | Inf -- to stdout and to log file
  | Progress -- only to stdout only if terminal is attached, not scroll "\rtext\t"
  | Summary -- only to sumary file
  | Error -- to stderr and to log file
  deriving (Show)

withLogger :: FilePath ->  FilePath -> (Log -> IO a) -> IO a
withLogger yabaLogPath sliceLogPath fce = do
  charOnLineCounter <- newIORef 0
  bufan <- hGetBuffering stdout
  withFile yabaLogPath AppendMode (\yhandle ->
    withFile sliceLogPath WriteMode (\shandle ->
      fce (doLog (LineBuffering == bufan) charOnLineCounter yhandle shandle)
     )
   )

-- fmap ((show l ++ ": "):) .
doLog :: Bool -> IORef Int -> Handle -> Handle -> Level -> String -> IO()
doLog isTerminal charOnLineCounter yh sh l s = do
    let ss = show l ++ ": " ++ s
        printToDebugLog = do
          time' <- getCurrentTime
          hPutStrLn sh $ (take 19 $ show time') ++ ": " ++ ss
          return ()
    case l of
      Debug -> do
        time' <- getCurrentTime
        printToDebugLog
      Inf -> do
        cleanLine
        putStrLn s
        hFlush stdout
        printToDebugLog
      Progress -> do
        when isTerminal $ do
          with <-  termWidth
          case with of
            Nothing -> return ()
            Just width -> do
              cleanLine
              let txt = take (width - 1) s
              writeIORef charOnLineCounter (length txt)
              putStr $ '\r' : txt ++ "\r"
              hFlush stdout
      Summary -> do
        time <- getCurrentTime
        hPutStrLn yh $ (take 19 . show $ time) ++ ": " ++ s
        hFlush yh
        printToDebugLog
      Error -> do
        hPutStrLn stderr $ s
        printToDebugLog
  where
    cleanLine = do -- cleaneng line of progress
      when isTerminal $ do
       charsToClean <- readIORef charOnLineCounter
       when (charsToClean > 0) $ do
           width <-  termWidth
           case width of
              Nothing -> return ()
              Just width -> do
                 putStr $ replicate (min charsToClean (width - 1)) ' ' ++ "\r"
                 writeIORef charOnLineCounter 0

termWidth :: IO (Maybe Int)
termWidth = fmap (fmap Terminal.width) Terminal.size
--termWidth = return (Just 100)

loa :: String -> IO ()
loa xx = do
   bufan <- hGetBuffering stdout
   let kon =  LineBuffering == bufan
   putStr $ show kon ++ ": "
   putStrLn xx
   hPutStrLn stderr xx

logInScan :: UTCTime -> Log -> EventEnvelop a ErrList -> IO ErrList
logInScan startTime lo (EventEnvelop revpath (Cumulative count' size' countSpeed sizeSpeed) event errList) = do
 case event of
   Ignore -> do
     lo Debug $ "IGNORE: " ++ pth revpath
     return errList
   Start rootPath -> do
     lo Debug $  "========================================================================="
     lo Debug $ "Start scanning: " ++ rootPath ++ " at " ++ show startTime
     return errList
   End _ _ -> do
     endTime <- getCurrentTime
     lo Debug $ "End scanning at " ++ show endTime ++ ", duration=" ++ show (duration endTime) ++ "; total: "
     -- not LN
     lo Debug $ printf "%6d# %10.3f MB | %9.2f #/s  %10.3f MB/s  " count' (sizeInMb size') countSpeed sizeSpeed
     return errList
   AfterFile (EventFile size)  _ -> do
     time' <- getCurrentTime
     -- lo Debug $  duration time'
     forM_ [lo Debug, lo Progress] ($  printf "%s %6d # %10.3f MB %9.2f #/s  %7.3f MB/s %10.3f %s"
         (duration time' ) count' (sizeInMb size') countSpeed sizeSpeed (sizeInMb size) (pth revpath))
     return errList
   BeforeFile (EventFile size) -> do
     when (size > 1024 * 1024 * 100) (do
       forM_ [lo Debug, lo Progress] ($ printf "  ... big file: %10.3f - %s" (sizeInMb size) (pth revpath))
       )
     return errList
   Failure exc -> do
     let errstr = "!!!!! ERROR !!!!! " ++ show exc
     lo Error $ "    " ++ errstr
     return  $ ErrList (errstr: getErrList errList)
   _ -> return errList

 where duration time' = take 6 (show (diffUTCTime time' startTime)) ++ "s "

-- getEventHandler lo  = (logInScan lo, getCurrentTime)