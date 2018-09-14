{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Yaba.Data.Lodree (
  Lodree(..),
  Ree(..),
  emptyLodree,
  hashLodree,
  makeLDir,

  currentLodree,
  findLodreeNode,
  ree,
  flattenFileLodree,
  takeRestoreTuples
) where

import           Control.Applicative
import           Control.Arrow
import qualified Crypto.Hash.SHA1      as Cr
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.UTF8  as BSU
import           Data.Function
import           Data.List
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Time.Clock.POSIX
import           Data.Tuple
import           Data.Yaml
import           GHC.Generics
import           System.Directory.Tree (DirTree (Dir, File), FileName)
import           System.FilePath

import           Lib
import           TurboWare
import           Types

-- import           Data.Time.Clock.UTC

millisToUTC :: Integer -> UTCTime
millisToUTC t = posixSecondsToUTCTime $ (fromInteger t) / 1000

data Lodree = LFile Ree FilePath
            | LDir Ree [(FileName, Lodree)]
            deriving (Show)

emptyLodree :: Lodree
emptyLodree = makeLDir []

makeLDir :: [(FileName, Lodree)] -> Lodree
makeLDir list' = LDir (foldToDree list) list
  where
    list = filter (not . isEmptyDir . snd) list'

    foldToDree :: [(FileName, Lodree)] -> Ree
    foldToDree list = let
        sortedList = sortBy (compare `on` fst) list
        names = map (BSU.fromString . fst) list
        hashes = map (pickHash . snd) sortedList
      in Ree {
               rsize = sum $ (pickSize . snd) <$> list,
               rcount = sum $ (pickCount . snd) <$> list,
               rtime = millisToUTC 0,
               rhash = Cr.finalize $ foldl Cr.update Cr.init (hashes ++ names)
            }-- emptyDRee = DRee 0 0 Strict.empty

currentLodree :: Lodree -> Lodree
currentLodree (LDir _ [])                 = emptyLodree
currentLodree (LDir _ ((_, current) : _)) = current

findLodreeNode = findNode

ree :: Lodree -> Ree
ree (LFile ree _) = ree
ree (LDir ree _)  = ree

pickSize :: Lodree -> FileSize
pickSize = rsize . ree

pickHash :: Lodree -> Hash
pickHash = rhash . ree

hashLodree = pickHash

pickCount :: Lodree -> Int
pickCount = rcount . ree

isEmptyDir :: Lodree -> Bool
isEmptyDir (LDir _ []) = True
isEmptyDir _           = False

findNode :: FilePath -> Lodree -> Maybe Lodree
findNode [] lodree = Just lodree
findNode "/" lodree = Just lodree
findNode path (LFile _ _) = error $ "Impossible has happend, we have found file but ther is some stuff in the path: " ++ path
findNode path (LDir _ list) = let
  (name, rest) = break ('/'==) (dropPrefixSlashes path)
  lodree2 = snd <$> find ((name==) . fst) list
  in lodree2 >>= findNode rest

flattenFileLodree :: Lodree -> [(RevPath, Ree)]
flattenFileLodree lodree = flan ([], lodree)
  where
    flan :: (RevPath, Lodree) -> [(RevPath, Ree)]
    flan (rp, (LFile ree _))  = [(rp, ree)]
    flan (rp, (LDir _ items)) = fmap (first (:rp)) items >>= flan

-- | take tuples to restore originalpath to logicalpath
takeRestoreTuples :: Lodree -> [(RevPath, FilePath)]
takeRestoreTuples lodree = x' ([], lodree)
  where
    x' :: (RevPath, Lodree) -> [(RevPath, FilePath)]
    x' (rp, (LFile _ originalPath)) = [(rp, originalPath)]
    x' (rp, (LDir _ items))         = fmap (first (:rp)) items >>= x'


--------------------------------------------------------
--
-- Instances for YAML


instance ToJSON Lodree where
   toJSON (LFile ree _) = toJSON ree
   toJSON (LDir _ list) = toJSON (M.fromList list)



--------------------------------------------------------
-- The rest of this modul is for DEBUGING purpose only - it is dump
--
instance Dumpable Lodree where
  -- toDump :: DirCompare -> [String]

  toDump :: Lodree -> [String]
  toDump (LFile ree _) = [printRee ree]
  toDump (LDir _ items) = ("    " ++) <$> (items >>= todump)
     where
        todump :: (FileName, Lodree) -> [String]
        todump (filename, q@(LFile _ _)) = prependToFirst (filename ++ ": ") (toDump q)
        todump (filename, q@(LDir ree _)) =   ("/" ++ filename ++ " " ++ printRee ree) : toDump q

printRee :: Ree ->  String
printRee Ree {..} = "  #" ++ show rsize ++ " " ++ toHexStr rhash


--w = do
--  putStrLn $ unlines $ lodreeToStringList (gen "abcd")
