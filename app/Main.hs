module Main where

import           Crypto.Hash.SHA1      (hashlazy)
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.Lazy  as Lazy
import           Data.Maybe
--import           Filesystem.Path
import           GHC.IO.Encoding
import           Lib
import           LogicalDirTree
import           System.Directory.Tree
import           System.FilePath.Find
import           Text.Printf           (printf)
import           TreeComparator
import           YabaDirTree           hiding (RegularFile)
import           YabaFileContent


hashFile :: FilePath -> IO Strict.ByteString
hashFile = fmap hashlazy . Lazy.readFile


src3 = "c:/!work"

hashniFile :: FilePath -> IO (FilePath, String)
hashniFile f = fmap (\h -> (f, toHexStr h)) (hashFile f)

hashniFiles :: [FilePath] -> IO [(FilePath, String)]
hashniFiles = mapM hashniFile

mainovec :: IO ()
mainovec = do
  putStrLn "Hello World"
  let vysl = find always (fileType ==? RegularFile) src3 >>= hashniFiles

  vysl >>=  putStrLn . unlines . map (\(x, y) -> x ++ " | " ++ y)

   -- src3 >>= putStrLn . unlines



main :: IO ()
main = do
  setLocaleEncoding utf8
  getLocaleEncoding >>= print
  -- putStrLn "→"


backup = "backupdisk1"

printLodree :: Lodree -> IO()
printLodree lodree = putStrLn $ (unlines . lodreeToStringList) lodree

ee = do
 (base1 :/ d1) <- readYabaDir "./test/data/compare1/left"
 (base2 :/ d2) <- readYabaDir "./test/data/compare1/right"
 -- putStrLn $ " ============ " ++ base1 ++ " | " ++ base2
 putStrLn  " ============ LEFT"
 let lodree1 = merge emptyLodree d1
 printLodree lodree1
 putStrLn  " ============ RIGHT"
 let lodree2 = merge emptyLodree d2
 printLodree lodree2
 putStrLn  " ============ COMPARE"
 let diff = compareTrees lodree2 lodree1
 putStrLn $ unlines $ dirCompareToStringList (fromJust diff)

ww = do
  (base :/ d) <- readYabaDir  $ "./test/data/" ++ backup ++ "/2018-02-03T00-00-00.yaba"
  putStrLn $ " ============ " ++ base
  putStrLn $ unlines $ yabaDirTreeToStringList d
  putStrLn " ============ "
  let lodree1 = merge emptyLodree d
  printLodree lodree1



  (base :/ d) <- readYabaDir $ "./test/data/" ++ backup ++ "/2018-02-04T00-00-00.yaba"
  putStrLn $ " ============= " ++ base
  putStrLn $ unlines $ yabaDirTreeToStringList d
  putStrLn " ============ "
  let lodree2 = merge lodree1 d
  printLodree lodree2

  let co = "maintree/M/OO/"
  -- let co = "/"
  putStrLn $ " ============ VYBER " ++ co
  printLodree $ fromJust $ findNode co lodree2
