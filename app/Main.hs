module Main where

import           Crypto.Hash.SHA1      (hashlazy)
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.Lazy  as Lazy
import           Data.Maybe
import qualified DirTree               as OldDirTree
import           DirTreeCompare
--import           Filesystem.Path
import           GHC.IO.Encoding
import           Lib
import           LogicalDirTree
import           System.Directory.Tree
import           System.FilePath.Find
import           Text.Printf           (printf)
import           YabaDirTree           hiding (RegularFile)
import           YabaFileContent


hashFile :: FilePath -> IO Strict.ByteString
hashFile = fmap hashlazy . Lazy.readFile


src1 = "../test/data/cd532/install"
src2 = "i:/zoje/compare-builds/test/data/cd536/install"
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


zastringuj :: Show a => (a -> String) -> DirTree2 a -> [String]
zastringuj f (AddFord dt) = ["+ " ++ fileNamex dt ++ " "]
zastringuj f (DeleteFord dt) = ["- " ++ fileNamex dt ++ " " ]
zastringuj f (Nic2 name a) = ["= " ++ name ++ " " ++ f a ]
zastringuj f this@(Dir2 name contents) = ("/ " ++ name
  ++ " #" ++ show (leftCount this) ++ "|" ++ show (rightCount this))
  : map ("   "++) (concat (zastringuj f <$> contents))

main :: IO ()
main = do
  setLocaleEncoding utf8
  getLocaleEncoding >>= print
  -- putStrLn "→"
  q

q :: IO ()
q = do
  (base1 :/ d1) <- OldDirTree.readSourceDir "./test/case1/left"
  (base2 :/ d2) <- OldDirTree.readSourceDir "./test/case1/right"
  let (Just sloz) = mergeTrees OldDirTree.sameFiles d1 d2
  putStrLn base1
  putStrLn base2
  putStrLn "///"
  putStrLn $ unlines $ zastringuj show sloz

backup = "backupdisk1"

printLodree :: Lodree -> IO()
printLodree lodree = putStrLn $ (unlines . lodreeToStringList) lodree

ww = do
  (base :/ d) <- readYabaDir  $ "./test/data/" ++ backup ++ "/2018-02-03T00-00-00.yaba"
  putStrLn $ " ============ " ++ base
  putStrLn $ unlines $ yabaDirTreeToStringList d
  putStrLn " ============ "
  let lodree1 = merge emptyLodree d
  printLodree lodree1



  (base :/ d) <- readYabaDir $ "./test/data/" ++ backup ++ "/2018-02-04T00-00-00.yaba"
  putStrLn $ " ============ " ++ base
  putStrLn $ unlines $ yabaDirTreeToStringList d
  putStrLn " ============ "
  let lodree2 = merge lodree1 d
  printLodree lodree2

  let co = "maintree/M/OO/"
  -- let co = "/"
  putStrLn $ " ============ VYBER " ++ co
  printLodree $ fromJust $ findNode co lodree2
