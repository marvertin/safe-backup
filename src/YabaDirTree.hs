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

import qualified Data.Map              as M
import           Data.Yaml
import           GHC.Generics



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
    (base :/ tree) <- sortDirShape </$> readDirectoryWith getFileInfo f
    let tree' = fmap (truncate (length base)) (base :/ removeFirstLevelFiles tree)
    if anyFailed tree then do
      putStrLn $ "  !!!!!!!!!!!!! Selhalo to: " ++ show (failures tree)
    else do
      putStrLn $ " OK  #" ++  show (totalFilesCount tree) ++ " / " ++ show (fromIntegral (totalDirSize tree) / 1024 / 1024 ) ++ " MB"
    return $ tree'
  where
    truncate :: Int -> FordInfo -> FordInfo
    truncate n this@ RegularFile {} = this { physPath = drop n (physPath this)}
    truncate _ x                    = x

hu (Dir name list) = putStr "\n   " >> putStr name >> putStr ": " >> print (length list)

removeFirstLevelFiles :: DirTree a -> DirTree a
removeFirstLevelFiles (Dir name contents) = Dir name $ filter (not . isFile) contents
   where
    isFile (File _ _) = True
    isFile _          = False

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


  --------------------------------------------------------
  --
  -- Instances for YAML

instance ToJSON (DirTree FordInfo) where
     toJSON (File _ x)   = toJSON x
     toJSON (Dir _ list) = toJSON $ M.fromList $ (tupl <$> list)
       where
        tupl q@(File name _) = (name, q)
        tupl q@(Dir name _)  = (name, q)

instance ToJSON FordInfo where
  toJSON RegularFile{..} = toJSON $ show fordSize ++ " " ++ toHexStr fileHash
  toJSON (YabaFile x)    = toJSON x
