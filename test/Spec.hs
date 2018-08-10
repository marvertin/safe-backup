

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

main :: IO ()
main = tee

tee = do
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


printLodree :: Lodree -> IO()
printLodree lodree = putStrLn $ (unlines . lodreeToStringList) lodree
