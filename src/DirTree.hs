module DirTree
    ( p, p2,
    readSourceDir,
    Hash,
    FileSize,
    FileInfo,
    HasFileName(fileNamex),
    sameFiles
    ) where

import           Lib
import           System.Directory
import           System.Directory.Tree
import           Text.Printf           (printf)

import qualified Crypto.Hash.SHA1      as Cr
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.ByteString.UTF8  as BSU

type FileSize = Integer

type Hash = Strict.ByteString
type FileInfo = (FilePath, FileSize, Hash)

class  HasFileName a  where
  fileNamex :: a -> FileName


instance HasFileName (DirTree a) where
  fileNamex (Dir name _)    = name
  fileNamex (File name _)   = name
  fileNamex (Failed name _) = name

hashFile :: FilePath -> IO Strict.ByteString
hashFile = fmap Cr.hashlazy . Lazy.readFile

hashDirTree :: DirTree FileInfo -> Hash
hashDirTree (File _ (_, _, hash)) = hash
hashDirTree (Dir _ subdirs) = let
  hashes = map hashDirTree subdirs
  names = map (BSU.fromString . fileNamex) subdirs
  in Cr.finalize $ foldl Cr.update Cr.init (hashes ++ names)

printFileInfo :: FileInfo -> String
printFileInfo (_, size, hash) = "#" ++ show size ++ " " ++ toHexStr hash

getFileInfo :: FilePath -> IO FileInfo
getFileInfo f = do
  size <- getFileSize f
  hash <- hashFile f
  return (f, size, hash)

zastringuj :: Show a =>  (a -> String) -> DirTree a -> [String]
zastringuj f (File name a) = [name ++ ": " ++ f a]
zastringuj f (Dir name contents) = name : map ("   "++) (concat (zastringuj f <$> contents))
zastringuj f (Failed name exc) = [name ++ "EXCE " ++ show exc]

zastringuj2 :: (FileInfo -> String) -> DirTree FileInfo -> [String]
zastringuj2 f (File name a) = [name ++ ": " ++ f a]
zastringuj2 f q@(Dir name contents) = ("/ " ++ name ++ ":  " ++ toHexStr (hashDirTree q)) : map ("   "++) (concat (zastringuj2 f <$> contents))
zastringuj2 f (Failed name exc) = [name ++ "EXCE " ++ show exc]

readSourceDir :: FilePath -> IO (AnchoredDirTree FileInfo)
readSourceDir f = sortDirShape </$> readDirectoryWith getFileInfo f

sameFiles :: FileInfo -> FileInfo -> Bool
sameFiles (_, _, hash1) (_, _, hash2) = hash1 == hash2

p :: IO ()
p = do
  (base :/ d2) <- readSourceDir  "./"
  putStrLn $ unlines $ zastringuj printFileInfo d2

p2 :: IO ()
p2 = do
    (base1 :/ d1) <- readSourceDir  "./test/case1/left"
    (base2 :/ d2) <- readSourceDir  "./test/case1/right"
    putStrLn $ "------- " ++ base1
    putStrLn $ unlines $ zastringuj2 printFileInfo d1
    putStrLn $ "------- " ++ base2
    putStrLn $ unlines $ zastringuj2 printFileInfo d2
