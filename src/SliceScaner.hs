{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module SliceScaner
    (
    readSlice,

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
import           Slice




readSlice :: FilePath -> IO AnchoredSliceTree
readSlice f = do
    putStr $ " - " ++ f ++ " ... "
    hFlush stdout
    (base :/ tree) <- sortDirShape </$> readDirectoryWith loadSliceFile f
    let tree' = fmap (truncate (length base)) (base :/ removeFirstLevelFiles tree)
    if anyFailed tree then do
      putStrLn $ "  !!!!!!!!!!!!! Selhalo to: " ++ show (failures tree)
    else do
      putStrLn $ " OK  #" ++  show (totalFilesCount tree) ++ " / " ++ show (fromIntegral (totalDirSize tree) / 1024 / 1024 ) ++ " MB"
    return $ tree'
  where
    truncate :: Int -> SliceFile -> SliceFile
    truncate n (RegularFile ree@Ree{}) = RegularFile $ ree { rphysPath = drop n (rphysPath ree)}
    truncate _ x                    = x



loadSliceFile :: FilePath -> IO SliceFile
loadSliceFile ff = do
  let f = replaceBacklashesToSlashes ff
  size <- getFileSize f
  hash <- computeFileHash f
  if not $ yabaSuffix `isSuffixOf` f then return (RegularFile $ Ree f 1 size hash)
          else (MetaFile . parseMetaFile . T.unpack) <$> TIO.readFile f



removeFirstLevelFiles :: DirTree a -> DirTree a
--removeFirstLevelFiles = id
removeFirstLevelFiles (Dir name contents) = Dir name $ filter (not . isFile) contents
   where
    isFile (File _ _) = True
    isFile _          = False




totalDirSize :: SliceTree -> FileSize
totalDirSize = sum . fmap size0
  where
    size0 :: SliceFile -> FileSize
    size0 (MetaFile _)          = 100 -- odhadnout
    size0 (RegularFile Ree{..})=rsize

totalFilesCount :: SliceTree -> Int
totalFilesCount = sum . fmap (const 1)
