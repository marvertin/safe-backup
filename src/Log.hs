module Log (
  withLogger,
  Level(..),
  Log,
  -- lo
) where

import           System.IO

type Log = Level -> String -> IO ()

data Level
  = Debug -- only to log file
  | Info -- to stdout and to log file
  | Progress -- only to stdout only if terminal is attached, not scroll "\rtext\t"
  | Summary -- only to sumary file
  | Error -- to stderr and to log file
  deriving (Show)

withLogger :: FilePath -> (Log -> IO a) -> IO a
withLogger path fce =
  withFile path WriteMode (\handle ->
    fce (doLog handle)
  )

doLog :: Handle -> Level -> String -> IO()
doLog h l s = do
   putStrLn $ (show l) ++ "-/: " ++ s
   hPutStrLn h $ (show l) ++ "-/Po: " ++ s

lo :: String -> IO ()
lo xx = do
   bufan <- hGetBuffering stdout
   let kon =  LineBuffering == bufan
   putStr $ show kon ++ ": "
   putStrLn xx
   hPutStrLn stderr xx
