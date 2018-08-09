{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

module YabaDirTree
    ( p3,
    FordInfo(RegularFile,YabaFile),

    readSourceDir,
    JabaContent,
    yabaDirTreeToStringList,
    removeYabaExtension
    ) where

import           Data.List
import           Data.Maybe
import           Dump
import           GHC.IO.Encoding
import           Lib
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           Text.Printf           (printf)
import           Types

import qualified Crypto.Hash.SHA1      as Cr
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.ByteString.UTF8  as BSU

data FordInfo = RegularFile { physPath :: FilePath, fordSize :: FileSize, fileHash :: Hash }
              | YabaFile JabaContent  -- file content
              deriving (Show)



hashFile :: FilePath -> IO Strict.ByteString
hashFile = fmap Cr.hashlazy . Lazy.readFile



printFordInfo :: FordInfo ->  Maybe String
printFordInfo RegularFile {..} = Just$ "#" ++ show fordSize ++ " " ++ toHexStr fileHash
printFordInfo (YabaFile content) = Just$ show $ "{" ++ content ++ "}"

printFordInfo2 :: FordInfo -> Maybe String
printFordInfo2 RegularFile {physPath} = Just physPath
printFordInfo2 (YabaFile _)           = Nothing

getFileInfo :: FilePath -> IO FordInfo
getFileInfo f = do
  size <- getFileSize f
  hash <- hashFile f
  if not $ yabaSuffix `isSuffixOf` f then return (RegularFile f size hash)
          else YabaFile <$> readFile f

removeYabaExtension :: FileName -> FileName
removeYabaExtension name = fromMaybe name (stripExtension yabaSuffix name)

readSourceDir :: FilePath -> IO (AnchoredDirTree FordInfo)
readSourceDir f = sortDirShape </$> readDirectoryWith getFileInfo f

yabaDirTreeToStringList = dirTreeToStringList printFordInfo2

proved :: String -> IO ()
proved s = do
  (base :/ tree) <- readSourceDir s
  putStrLn $ "************** <" ++ s ++ "> | <" ++ base ++ ">"
  putStrLn $ unlines $ dirTreeToStringList printFordInfo2 tree


p3 :: IO ()
p3 = do
    setLocaleEncoding utf8
    getLocaleEncoding >>= print


    --proved  "./test/data/yaba-dir-tree-1"
    --putStrLn $ "------- " ++ base
    --putStrLn $ unlines $ dirTreeToStringList printFordInfo tree
    --putStrLn $ "------- " ++ base

    proved  "S:/"
    proved  "S:\\"
    proved  "S:"
    proved  "./test/data/yaba-dir-tree-1"
    proved  "./test/data/yaba-dir-tree-1\\"
    proved  ".\\test\\data\\yaba-dir-tree-1"
    proved  ".\\test\\data\\yaba-dir-tree-1\\"
