module Log (
  lo
) where

import           System.IO
type Log = String -> IO ()

lo :: Log
lo xx = do
   bufan <- hGetBuffering stdout
   let kon =  LineBuffering == bufan
   putStr $ show kon ++ ": "
   putStrLn xx
   hPutStrLn stderr xx
