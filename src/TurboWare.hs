module TurboWare
    (
      tupleMaybeUpFst,
      tupleMaybeUpSnd,
      replaceBacklashesToSlashes,
      replaceVerticalToSlashes,
      replaceSlashesToVertical,
      zipMaybe,
      prependToFirst,
      appendToFirst,
      dropPrefixSlashes,
      trim,
      safeHead,
      replaceItem,
      createDirectories,
      Dumpable(..),
      Hexable(..)
    ) where

import qualified Data.ByteString  as Strict
import           Data.Char
import           Data.Function
import           Data.List        (sortBy)
import           System.Directory
import           System.FilePath
import           Text.Printf      (printf)

{- | Dump to lines for debug purpures.
-}
class Dumpable a where
  toDump :: a -> [String]

  toDumpS :: a -> String
  toDumpS = unlines . toDump

  dump :: a -> IO ()
  dump = putStrLn . toDumpS

class  Hexable a  where
  toHexStr :: a -> String

instance Hexable Strict.ByteString where
  toHexStr bytes = Strict.unpack bytes >>= printf "%02x"

dropPrefixSlashes :: FilePath -> FilePath
dropPrefixSlashes []         = []
dropPrefixSlashes ('/' : x)  = dropPrefixSlashes x
dropPrefixSlashes ('\\' : x) = dropPrefixSlashes x
dropPrefixSlashes x          = x

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


replaceVerticalToSlashes :: String -> String
replaceVerticalToSlashes = let
      repl '|' = '/'
      repl x   = x
  in map repl

replaceSlashesToVertical :: String -> String
replaceSlashesToVertical = let
        repl '/'  = '|'
        repl '\\' = '|'
        repl x    = x
    in map repl

-- | Prepends list to the first list of list of list
-- | IF list of list is empty return singleton list of first list
prependToFirst :: [a] -> [[a]] -> [[a]]
prependToFirst [] []     = []
prependToFirst x []      = [x]
prependToFirst x (y: ys) = (x ++ y) : ys

-- | Appends list to the first list of list of list
-- | IF list of list is empty return singleton list of first list
appendToFirst :: [a] -> [[a]] -> [[a]]
appendToFirst [] []     = []
appendToFirst x []      = [x]
appendToFirst x (y: ys) = (y ++ x) : ys

-- | O(n (ln n)) Takes two functios which creates Ordable keys from two Types
-- | Then takes two lists whic zips by result of the two functions
-- | The result is sorted by the key
-- | zipMaybe (*3) length [1, 2] ["qwer", "abcABC"] = [(3,Just 1,Nothing),(4,Nothing,Just "qwer"),(6,Just 2,Just "abcABC")]
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

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

safeHead :: a -> [a] -> a
safeHead def [] = def
safeHead _ l    = head l

-- | Create directory witch parent directories but only if not exists
createDirectories :: FilePath -> IO ()
createDirectories dir = do
   createDirectoryIfMissing True (takeDirectory dir)
   createDirectory dir

replaceItem :: Int -> a -> [a] ->[a]
replaceItem index item list =
    let (pref, suf) = splitAt index list
    in if null suf then list
                   else pref ++ [item] ++ tail suf
