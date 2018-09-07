{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module SliceNameStrategy (
  SliceNameStrategy,
  defaultSliceNameStrategy,

  listSlices,
  nextSliceName,
  listDirSortedN,
  nextn,
) where

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Yaml
import           GHC.Generics
import           System.Directory
import           System.FilePath
import           System.FilePath.Find
import           Text.Printf

import           TurboWare

defaultSliceNameStrategy = UtcSeconds

data SliceNameStrategy = UtcSeconds | UtcSecondsByYear | UtcSecondsByYearMonth | UtcSecondsByMinute |
                         Numbers4
   deriving (Show, Generic, FromJSON, ToJSON)


listSlices :: SliceNameStrategy -> FilePath -> IO [FilePath]
listSlices sliceNameStrategy path = fmap replaceSlashesToVertical <$> listSlices' path
 where
  listSlices' = listDirSortedN $
     case sliceNameStrategy of
       UtcSeconds            -> 1
       UtcSecondsByYear      -> 2
       UtcSecondsByYearMonth -> 2
       UtcSecondsByMinute    -> 2
       Numbers4              -> 2

listDirSorted :: FilePath -> IO [FilePath]
listDirSorted dir = sort <$> listDirectory dir

listDirSortedN :: Int -> FilePath -> IO [FilePath]
listDirSortedN n rootDir = fmap replaceBacklashesToSlashes <$> listd n ""
  where
     listd :: Int -> FilePath -> IO [FilePath]
     listd 0 x   = return [x]
     listd n dir = do
        names <- listDirSorted (rootDir </> dir)
        concat <$> mapM (listd (n - 1) . (dir </>)) names

nextSliceName :: FilePath -> SliceNameStrategy -> IO FilePath
nextSliceName sliceRoot strategy = do
  time <- currentUtcTimeFormatted
  return $ case strategy of
             UtcSeconds            -> time
             UtcSecondsByYear      -> replaceItem 4 '|' time
             UtcSecondsByYearMonth -> replaceItem 7 '|' time
             UtcSecondsByMinute    -> replaceItem 16 '|' time

currentUtcTimeFormatted :: IO String
currentUtcTimeFormatted = do
  now <- getCurrentTime
  return $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H-%M-%SZ")) now


-- | next number with numerical and lexicoraphical order
nextn :: String -> String
nextn whole@(letter : digits)
  | isDigit letter = dropWhile (== pred 'a') . nextn $ pred 'a' : whole
  | otherwise =
      let newNum = 1 + read digits :: Int
          digits2 = show newNum
      in if length digits2 > length digits
         then (succ letter) : reverse digits2
         else letter : printf ("%0" ++ show (length digits) ++ "d") newNum

buildSliceName :: String -> String -> String
buildSliceName [] _ = []
buildSliceName ('?': xs) stime =
    let (nums, rest) = spanNums stime
    in nums ++ buildSliceName xs rest
buildSliceName (char: xs) stime = char : buildSliceName xs stime

spanNums :: String -> (String, String)
spanNums s = let (x, y) = span isDigit s
             in (x, dropWhile (not . isDigit) y)
