{-# LANGUAGE RecordWildCards #-}

module Util.Lib
    (
      HasFileName(..),
      mapTree,
      namesToPath,
      deAnchore,
      splitByChar,
      computeFileHash,
      sizeInMb,
      showSz,
      showDiffTm,
      zipPaths',
      pth

    ) where

import           Control.Arrow
import           Control.Exception
import           Control.Monad
import qualified Crypto.Hash.SHA1      as Cr
import qualified Data.ByteString       as Strict
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.ByteString.UTF8  as BSU
import           Data.Either
import           Data.Function
import           Data.List             (intercalate, nub, sortBy)
import           Data.List.Unique
import           Data.Maybe
import           Data.Time.Clock
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           Text.Printf           (printf)


import           Util.TurboWare
import           Util.Types



class  HasFileName a  where
  fileNamex :: a -> FileName


instance HasFileName (DirTree a) where
  fileNamex (Dir name _)    = name
  fileNamex (File name _)   = name
  fileNamex (Failed name _) = name

mapTree :: ([FileName] -> a -> b) -> DirTree a -> DirTree b
mapTree  =
  let
    mapa :: [FileName] -> ([FileName] -> a -> b) -> DirTree a -> DirTree b
    mapa path fun (File name x)   = File name (fun (name:path) x)
    mapa path fun (Dir name list) = Dir name $  map (mapa (name:path) fun) list
  in mapa []

namesToPath :: [FileName] -> FilePath
namesToPath list = "/" ++ intercalate "/" (reverse list)

deAnchore :: AnchoredDirTree a -> DirTree a
deAnchore (_ :/ dirtree) = dirtree

removeFirstChar :: Char -> String -> String
removeFirstChar _ [] = []
removeFirstChar c q@(x: xs)
  | x == c = xs
  | otherwise = q

splitByChar :: Char -> String -> (String, String)
splitByChar z xs = let (s1, s2) = span (/=z) xs
                   in (trim s1, trim . removeFirstChar z $ s2)


-- | Compute hask of the file on filesystem
computeFileHash :: FilePath -> IO Strict.ByteString
computeFileHash = (fmap Cr.hashlazy . Lazy.readFile)  >=> evaluate

sizeInMb :: Integer -> Double
sizeInMb x =  fromIntegral x / 1024 / 1024

showSz ::  Integral a  => a -> String
showSz sz = let (x, m) = head . dropWhile (\(q, _) ->  q > 1024.0)
                         $ zip (qs (fromIntegral sz)) ["B", "KiB", "MiB", "GiB", "TiB", "PiB"]
            in printf "%4.3f %s" (x :: Double) m
  where qs size = size : qs (size / 1024.0)


showDiffTm :: UTCTime -> UTCTime -> String
showDiffTm endTime startTime = show (diffUTCTime endTime startTime)

zipPaths' :: DirTree a -> DirTree (FilePath, a)
zipPaths' (Dir _ list) = let dir = Dir "" list
                         in (first replaceBacklashesToSlashes) <$> zipPaths ("" :/ dir)

-- | convert reverse path to forward path not starting with slash
-- | pth ["yaba", "home", "opt"] == opt/home/jaba
pth :: RevPath -> FilePath
pth = foldl (flip (</>)) []
