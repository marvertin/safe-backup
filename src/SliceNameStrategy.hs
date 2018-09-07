{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module SliceNameStrategy (
  SliceNameStrategy,
  defaultSliceNameStrategy,

  listSlices,
  nextSliceName,
  listDirSortedN,
  nextn,
  lastSliceLatsPartName,
) where

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
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

defaultSliceNameStrategy = Pattern "*-*/*-*"

data SliceNameStrategy = Pattern String
   deriving (Show, Generic, FromJSON, ToJSON)

numberOfParts :: SliceNameStrategy -> Int
numberOfParts (Pattern pat) = length (filter (=='/') pat) + 1

listSlices :: SliceNameStrategy -> FilePath -> IO [FilePath]
listSlices sns path = fmap replaceSlashesToVertical <$> listSlices' path
 where
  listSlices' = listDirSortedN $ numberOfParts sns

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

-- | lastSliceLatPartName 3 ["aa/xx/11", "bb/yy/222"] = Just "222"
lastSliceLatsPartName :: Int -> FilePath -> IO (Maybe String)
lastSliceLatsPartName n root =
    fmap (lastPart . replaceSlashesToVertical ) . listToMaybe . reverse <$> listDirSortedN n root
  where lastPart = reverse . takeWhile (/='|') . reverse

nextSliceName :: FilePath -> SliceNameStrategy -> IO String
nextSliceName sliceRoot (Pattern pat) = do
  time <- currentUtcTimeFormatted
  return $ replaceSlashesToVertical $ buildSliceName pat time

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

-- | "y*M*/*-*xx/0001" "2042-12-17T23-15-47" = "y2042M12/17-23xx/0001"
buildSliceName :: String -> String -> String
buildSliceName [] _ = []
buildSliceName ('*': xs) stime =
    let (nums, rest) = spanNums stime
    in nums ++ buildSliceName xs rest
buildSliceName (char: xs) stime = char : buildSliceName xs stime

spanNums :: String -> (String, String)
spanNums s = let (x, y) = span isDigit s
             in (x, dropWhile (not . isDigit) y)
