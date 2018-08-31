module DirScan (
  scanDirectory,
) where

import           BackupTreeBuilder
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

type Acum = (Int, Integer)

scanDirectory :: FilePath -> IO ()
scanDirectory path = do
    putStrLn ""
    scanDirectory' 0 (0,0) path >>= print
    putStrLn ""

scanDirectory' :: Int -> Acum -> FilePath -> IO Acum
scanDirectory' level acum@(x, y) path = do
  -- putStrLn $ "Pokus: " ++ path
  isDir <- doesDirectoryExist path
  --putStr $  "\r" ++ show level ++ ": " ++ show acum ++ "  " ++ replicate (level * 4) ' ' ++ path
  let sajza = (fromIntegral y / 1024 / 1024) :: Double
  printf "%2d #%6d %10.3f MB %s \n" level x sajza path
  if isDir then do
      fords <- fmap (fmap (path </>)) (listDirectory path)
      foldM (scanDirectory' (level + 1)) acum fords
   else do
    -- xsize <- getFileSize path
     dataa <- Lazy.readFile path
     let size = Lazy.length dataa
     return (x + 1, y + fromIntegral size)
