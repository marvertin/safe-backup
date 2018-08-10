module Lib
    ( toHexStr,
      yabaSuffix,
      tupleMaybeUpFst,
      tupleMaybeUpSnd,
      replaceBacklashesToSlashes,
      zipMaybe,
      HasFileName(fileNamex),

    ) where

import qualified Data.ByteString       as Strict
import           Data.Function
import           Data.List             (sortBy)
import           System.Directory.Tree (DirTree (Dir, Failed, File), FileName)
import           Text.Printf           (printf)



yabaSuffix = ".yaba"

class  Hex a  where
  toHexStr :: a -> String

instance Hex Strict.ByteString where
  toHexStr bytes = Strict.unpack bytes >>= printf "%02x"

class  HasFileName a  where
  fileNamex :: a -> FileName


instance HasFileName (DirTree a) where
  fileNamex (Dir name _)    = name
  fileNamex (File name _)   = name
  fileNamex (Failed name _) = name


dropPrefixSlash :: FilePath -> FilePath
dropPrefixSlash ('/' : x)  = x
dropPrefixSlash ('\\' : x) = x
dropPrefixSlash x          = x

tupleMaybeUpFst :: (Maybe a, b) -> Maybe (a, b)
tupleMaybeUpFst (Nothing, _) = Nothing
tupleMaybeUpFst (Just a, b)  = Just (a, b)

tupleMaybeUpSnd :: (a, Maybe b) -> Maybe (a, b)
tupleMaybeUpSnd (_, Nothing) = Nothing
tupleMaybeUpSnd (a, Just b)  = Just (a, b)

replaceBacklashesToSlashes :: String -> String
replaceBacklashesToSlashes = let
      repl '\\' = '/'
      repl x    = x
  in map repl

zipMaybe :: Ord k => (a -> k) -> (b -> k) -> [a] -> [b] -> [(k, Maybe a, Maybe b)]
zipMaybe af bf al bl = merge (sortBy (compare `on` af) al)
                             (sortBy (compare `on` bf) bl)
    where
     -- merge :: [a] -> [b] -> [(k, Maybe a, Maybe b)]
     merge [] [] = []
     merge (a: as) [] = (af a, Just a, Nothing) :  merge as []
     merge [] (b: bs) = (bf b, Nothing, Just b) :  merge [] bs
     merge aq@(a: as) bq@(b: bs) = let
           ak = af a
           bk = bf b
        in case ak `compare` bk of
          LT -> (ak, Just a, Nothing) : merge as bq
          GT -> (bk, Nothing, Just b) : merge aq bs
          EQ -> (ak, Just a, Just b) : merge as bs
