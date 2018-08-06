module Main where

import           Lib
import DirTree

import           Crypto.Hash.SHA1     (hashlazy)
import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy
-- import           Filesystem.Path
import           System.FilePath.Find
import           Text.Printf          (printf)

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



main :: IO ()
main = mainovec
