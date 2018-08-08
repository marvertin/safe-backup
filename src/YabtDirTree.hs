{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

module YabtDirTree
    ( p3,
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

data FordInfo = RegularFile { physPath :: FilePath, fordSize :: FileSize, fileHash :: Hash }
              | YabtFile String  -- file content
              deriving (Show)

class  HasFileName a  where
  fileNamex :: a -> FileName


instance HasFileName (DirTree a) where
  fileNamex (Dir name _)    = name
  fileNamex (File name _)   = name
  fileNamex (Failed name _) = name

hashFile :: FilePath -> IO Strict.ByteString
hashFile = fmap Cr.hashlazy . Lazy.readFile



printFileInfo :: FordInfo -> String
printFileInfo RegularFile {..} = "#" ++ show fordSize ++ " " ++ toHexStr fileHash

getFileInfo :: FilePath -> IO FordInfo
getFileInfo f = do
  size <- getFileSize f
  hash <- hashFile f
  if True then return (RegularFile f size hash)
          else return (YabtFile "str")

zastringuj :: Show a =>  (a -> String) -> DirTree a -> [String]
zastringuj f (File name a) = [name ++ ": " ++ f a]
zastringuj f (Dir name contents) = ("/ " ++ name) : map ("   "++) (concat (zastringuj f <$> contents))
zastringuj f (Failed name exc) = [name ++ "EXCE " ++ show exc]

--zastringuj2 :: (FordInfo -> String) -> DirTree FordInfo -> [String]
--zastringuj2 f (File name a) = [name ++ ": " ++ f a]
--zastringuj2 f q@(Dir name contents) = ("/ " ++ name ++ ":  " ) : map ("   "++) (concat (zastringuj2 f <$> contents))
--zastringuj2 f (Failed name exc) = [name ++ "EXCE " ++ show exc]

readSourceDir :: FilePath -> IO (AnchoredDirTree FordInfo)
readSourceDir f = sortDirShape </$> readDirectoryWith getFileInfo f


p :: IO ()
p = do
  (base :/ d2) <- readSourceDir  "./"
  putStrLn $ unlines $ zastringuj printFileInfo d2

p3 :: IO ()
p3 = do
    (base1 :/ d1) <- readSourceDir  "./test/case1/left"
    (base2 :/ d2) <- readSourceDir  "./test/case1/right"
    putStrLn $ "------- " ++ base1
    putStrLn $ unlines $ zastringuj printFileInfo d1
    putStrLn $ "------- " ++ base2
    putStrLn $ unlines $ zastringuj printFileInfo d2
