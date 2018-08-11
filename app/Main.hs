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
import           TurboWare
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

ee = do
 (base1 :/ d1) <- readYabaDir "./test/data/compare1/left"
 (base2 :/ d2) <- readYabaDir "./test/data/compare1/right"
 -- putStrLn $ " ============ " ++ base1 ++ " | " ++ base2
 putStrLn  " ============ LEFT"
 let lodree1 = merge emptyLodree d1
 dump lodree1
 putStrLn  " ============ RIGHT"
 let lodree2 = merge emptyLodree d2
 dump lodree2
 putStrLn  " ============ COMPARE"
 let diff = compareTrees lodree2 lodree1
 dump (fromJust diff)

ww = do
  (base :/ d) <- readYabaDir  $ "./test/data/" ++ backup ++ "/2018-02-03T00-00-00.yaba"
  putStrLn $ " ============ " ++ base
  dump d
  putStrLn " ============ "
  let lodree1 = merge emptyLodree d
  dump lodree1

  (base :/ d) <- readYabaDir $ "./test/data/" ++ backup ++ "/2018-02-04T00-00-00.yaba"
  putStrLn $ " ============= " ++ base
  dump d
  putStrLn " ============ "
  let lodree2 = merge lodree1 d
  dump lodree2

  let co = "maintree/M/OO/"
  -- let co = "/"
  putStrLn $ " ============ VYBER " ++ co
  dump $ fromJust $ findNode co lodree2
