{-# LANGUAGE QuasiQuotes #-}

module Debug (
  q, w, p3, mainovec, e, rr, (>:), (<:), (?:), Test(..), ww
) where

import           Crypto.Hash.SHA1      (hashlazy)
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.Lazy  as Lazy
import           Data.Maybe
--import           Filesystem.Path
import           Backup
import           BackupTreeBuilder
import           Dump
import           GHC.IO.Encoding
import           Hashpairing
import           Lib
import           LogicalDirTree
import           SourceTree
import           System.Directory.Tree
import           System.FilePath.Find
import           Text.Printf           (printf)
import           Text.RawString.QQ
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

printYabaDir :: String -> IO ()
printYabaDir s = do
 (base :/ tree) <- readYabaDir s
 putStrLn $ "************** <" ++ s ++ "> | <" ++ base ++ ">"
 dump tree

backupname = "backupdisk1"

p3 :: IO ()
p3 = do
    setLocaleEncoding utf8
    getLocaleEncoding >>= print

    -- proved  "S:/"
    -- proved  "S:\\"
    -- proved  "S:"
    printYabaDir  "./test/data/yaba-dir-tree-1"
    printYabaDir  "./test/data/yaba-dir-tree-1\\"
    printYabaDir  ".\\test\\data\\yaba-dir-tree-1"
    printYabaDir  ".\\test\\data\\yaba-dir-tree-1\\"



q = do
  (base :/ d) <- readYabaDir  $ "./test/data/" ++ backupname ++ "/2018-02-03T00-00-00.yaba"
  putStrLn $ " ============ " ++ base
  dump d
  putStrLn " ============ "
  let lodree1 = mergeToLodree emptyLodree d
  dump lodree1

  (base :/ d) <- readYabaDir $ "./test/data/" ++ backupname ++ "/2018-02-04T00-00-00.yaba"
  putStrLn $ " ============= " ++ base
  dump d
  putStrLn " ============ "
  let lodree2 = mergeToLodree lodree1 d
  dump lodree2

  let co = "maintree/M/OO/"
  -- let co = "/"
  putStrLn $ " ============ VYBER " ++ co
  dump $ fromJust $ findLodreeNode co (currentLodree lodree2)

  putStrLn  " ============ HASHPAIRS - phys"
  dump $ createFhysicalHashMap lodree2

  putStrLn  " ============ HASHPAIRS - logical"
  dump $ createLogicalHashMap lodree2

w = do
   (base1 :/ d1) <- readYabaDir "./test/data/compare1/left"
   (base2 :/ d2) <- readYabaDir "./test/data/compare1/right"
   -- putStrLn $ " ============ " ++ base1 ++ " | " ++ base2
   putStrLn  " ============ LEFT"
   let lodree1 = mergeToLodree emptyLodree d1
   dump lodree1
   putStrLn  " ============ RIGHT"
   let lodree2 = mergeToLodree emptyLodree d2
   dump lodree2
   putStrLn  " ============ COMPARE"
   let diff = compareTrees (currentLodree lodree1) (currentLodree lodree2)
   dump (fromJust diff)

ww  = do
  (base1 :/ d1) <- readYabaDir "./test/data/double/backup/2018-02-03T00-00-00Z.yaba-slice"
  (base2 :/ d2) <- readYabaDir "./test/data/double/backup/2018-08-29T07-13-19Z.yaba-slice"
  let lodree1 = mergeToLodree emptyLodree d1
  putStrLn  " ============ FIRST MERGE"
  let lodree2a = mergeToLodree lodree1 d2
  dump lodree2a
  putStrLn  " ============ SECOND MERGE"
  let lodree2b = mergeToLodree lodree2a d2
  dump lodree2b
  putStrLn  " ============ COMPARE"
  let diff = compareTrees (currentLodree lodree2a) (currentLodree lodree2b)
  dump (fromJust diff)

e = do
  putStrLn  " ============ LBACKUP lodreeBackupCurrent 22"
  lodreeBackupAll <- readBackupDir "./test/data/case4/backup"
  let lodreeBackupCurrent = currentLodree lodreeBackupAll
  dump lodreeBackupCurrent
  putStrLn  " ============ SOURCE lodreeSourceAllNodes"
  lodreeSourceOneNode <- readSourceTree "./test/data/case4/source-of-maintree"
  let lodreeSourceAllNodes = makeLDir [("maintree", lodreeSourceOneNode)]
  dump lodreeSourceAllNodes
  putStrLn  " ============ COMPARE"
  let diff = compareTrees lodreeBackupCurrent lodreeSourceAllNodes
  dump (fromJust diff)

  putStrLn  " ============ HASHPAIRS - physical - lodreeBackupAll"
  dump $ createFhysicalHashMap lodreeBackupAll

  putStrLn  " ============ HASHPAIRS - logical - lodreeBackupAll"
  dump $ createLogicalHashMap lodreeBackupAll

  putStrLn $ unlines $ dirTreeToStringList (Just . toDumpS) $
    fromJust $ buildBackup lodreeBackupAll lodreeSourceAllNodes "POKUSNYBEKUP"

rr = do
    let backupDir = "./test/data/case3/backup"
    let sourceOfMainTree = "./test/data/case3/source-of-maintree"
    backup backupDir [("maintree", sourceOfMainTree)]
    return ()


data Test = Test String deriving (Eq, Show)

(>:) :: Test -> Test -> Test
(Test a) >: (Test b) = Test $ "(" ++ a ++ " >: " ++ b ++ ")"

(<:) :: Test -> Test -> Test
(Test a) <: (Test b) = Test $ "(" ++ a ++ " <: " ++ b ++ ")"

(?:) :: Test -> Test -> Test
(Test a) ?: (Test b) = Test $ "(" ++ a ++ " ?: " ++ b ++ ")"

infix 6 ?:
infixr 6 >:
infixl 6 <:

--  lodree <- readBackupDir "./test/data/backupdisk1"
--  dump lodree
