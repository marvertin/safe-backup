{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module YabaDirTree
    (
    YabaDirTree,
    FordInfo(..),

    readYabaDir,
    JabaContent,
    isYabaFile,
    ) where

import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Dump
import           GHC.IO.Encoding
import           Lib
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO             (hFlush, stdout)
import           Text.Printf           (printf)
import           TurboWare
import           Types

import qualified Crypto.Hash.SHA1      as Cr
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.ByteString.UTF8  as BSU

type YabaDirTree = DirTree FordInfo

data FordInfo = RegularFile { physPath :: FilePath, fordSize :: FileSize, fileHash :: Hash }
              | YabaFile JabaContent  -- file content
              deriving (Show)

isYabaFile :: DirTree FordInfo -> Bool
isYabaFile (File _ (YabaFile _)) = True
isYabaFile _                     = False

hashFile :: FilePath -> IO Strict.ByteString
hashFile = (fmap Cr.hashlazy . Lazy.readFile)  >=> evaluate



printFordInfo :: FordInfo ->  Maybe String
printFordInfo RegularFile {..} = Just$ "  #" ++ show fordSize ++ " " ++ toHexStr fileHash ++ " \"" ++ physPath ++ "\""
printFordInfo (YabaFile content) = Just$  "{ " ++ intercalate " | " (lines $ unJabaContent content) ++ " }"

printFordInfo2 :: FordInfo -> Maybe String
printFordInfo2 RegularFile {physPath} = Just physPath
printFordInfo2 (YabaFile _)           = Nothing

getFileInfo :: FilePath -> IO FordInfo
getFileInfo ff = do
  let f = replaceBacklashesToSlashes ff
  size <- getFileSize f
  hash <- hashFile f
  if not $ yabaSuffix `isSuffixOf` f then return (RegularFile f size hash)
          else (YabaFile . JabaContent . T.unpack) <$> TIO.readFile f

--removeYabaExtension :: FileName -> FileName
--removeYabaExtension name = fromMaybe name (stripExtension yabaSuffix name)

readYabaDir :: FilePath -> IO (AnchoredDirTree FordInfo)
readYabaDir f = do
    putStr $ " - " ++ f ++ " ... "
    hFlush stdout
    tree@(base :/ d) <- sortDirShape </$> readDirectoryWith getFileInfo f
    if anyFailed d then do
      putStrLn $ "  !!!!!!!!!!!!! Selhalo to: " ++ show (failures d)
      return $ fmap (truncate (length base)) tree
    else do
      putStrLn $ " OK  #" ++  show (totalFilesCount d) ++ " / " ++ show (fromIntegral (totalDirSize d) / 1024 / 1024 ) ++ " MB"
      return $ fmap (truncate (length base)) tree
  where
    truncate :: Int -> FordInfo -> FordInfo
    truncate n this@ RegularFile {} = this { physPath = drop n (physPath this)}
    truncate _ x                    = x


totalDirSize :: YabaDirTree -> FileSize
totalDirSize = sum . fmap size0
  where
    size0 :: FordInfo -> FileSize
    size0 (YabaFile x)           = genericLength $ unJabaContent x
    size0 RegularFile {fordSize} =fordSize

totalFilesCount :: YabaDirTree -> Int
totalFilesCount = sum . fmap (const 1)


instance Dumpable (DirTree FordInfo) where
  toDump = dirTreeToStringList printFordInfo
