module DirTree
    ( p,
    readSourceDir,
    Hash,
    FileSize,
    FileInfo
    ) where

import Lib
import System.Directory.Tree
import System.Directory
import           Text.Printf          (printf)

import           Crypto.Hash.SHA1     (hashlazy)
import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy

type FileSize = Integer

type Hash = String
type FileInfo = (FilePath, FileSize, Hash)

hashFile :: FilePath -> IO Strict.ByteString
hashFile = fmap hashlazy . Lazy.readFile

getFileInfo :: FilePath -> IO (FilePath, Integer, String)
getFileInfo f = do
  size <- getFileSize f
  hash <- toHexStr <$> hashFile f
  return (f, size, hash)

zastringuj :: Show a => DirTree a -> [String]
zastringuj (File name a) = [name ++ ": " ++ show a]
zastringuj (Dir name contents) = name : map ("   "++) (concat (zastringuj <$> contents))

readSourceDir :: FilePath -> IO (AnchoredDirTree FileInfo)
readSourceDir f = sortDirShape </$> readDirectoryWith getFileInfo f

p :: IO ()
p = do
  (base :/ d2) <- readSourceDir  "./"
  putStrLn $ unlines $ zastringuj d2
