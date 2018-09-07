{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Debug (
  q, w, p3, mainovec, e, (>:), (<:), (?:), Test(..), ww, cc, ee, qq, yy, t, tt, l
) where

import           Crypto.Hash.SHA1           (hashlazy)
import qualified Data.ByteString            as Strict
import qualified Data.ByteString.Lazy       as Lazy
import           Data.Maybe
import           Log
--import           Filesystem.Path
import           Backup
import           BackupTreeBuilder
import           Config
import           Control.Monad
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe
import           DirScan
import           Dump
import qualified GHC.IO.Encoding            as GIE
import           Hashpairing
import           Ignorances
import           Lib
import           Lodree
import           SliceScaner                hiding (RegularFile)
import           SliceToLodree
import           SourceTree
import           System.Directory.Tree
import           System.FilePath.Find
import           System.IO
import           Text.Printf                (printf)
import           Text.RawString.QQ
import           Tree
import           TreeComparator
import           TurboWare

import qualified Data.ByteString.Char8      as B8
--import qualified Data.HashMap.Strict   as HM
import qualified Data.Map                   as M
--import           Data.Text
import           Data.Yaml
import           GHC.Generics

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
 (base :/ tree) <- readSlice s
 putStrLn $ "************** <" ++ s ++ "> | <" ++ base ++ ">"
 dump tree

backupname = "backupdisk1"

p3 :: IO ()
p3 = do
    GIE.setLocaleEncoding GIE.utf8
    GIE.getLocaleEncoding >>= print

    -- proved  "S:/"
    -- proved  "S:\\"
    -- proved  "S:"
    printYabaDir  "./test/data/yaba-dir-tree-1"
    printYabaDir  "./test/data/yaba-dir-tree-1\\"
    printYabaDir  ".\\test\\data\\yaba-dir-tree-1"
    printYabaDir  ".\\test\\data\\yaba-dir-tree-1\\"



q = do
  (base1 :/ d1) <- readSlice $ "./test/data/hashing/slice1"
  putStrLn $ " ============ " ++ base1
  dump d1
  putStrLn " ============ "
  let lodree1 = mergeToLodree emptyLodree d1
  dump lodree1

  (base2 :/ d2) <- readSlice $ "./test/data/hashing/slice2"
  putStrLn $ " ============ " ++ base2
  dump d2
  putStrLn " ============ "
  let lodree2 = mergeToLodree lodree1 d2
  dump lodree2


  putStrLn  " ============ HASHPAIRS"
  dump $ createMapOfHashes lodree2

  putStrLn  " ============ HASHPAIRS'"
  dump $ createMapOfHashes' lodree2


  forM_ ["/slice1/přesunout-do-noveho", "blb", "/slice1/SMAZATI"] (\co -> do
      putStrLn $ " ============ VYBER " ++ co
      case findLodreeNode co (lodree2) of
        Just x  -> dump x
        Nothing ->   putStrLn "NIC"
    )


qq = do
  (base :/ d) <- readSlice  $ "./test/data/" ++ backupname ++ "/2018-02-03T00-00-00.yaba"
  putStrLn $ " =====NOVE======= " ++ base
  dump d
  --(base :/ d) <- readSlice'  $ "./test/data/" ++ backupname ++ "/2018-02-03T00-00-00.yaba"
  --putStrLn $ " =====STARE======= " ++ base
  --dump d

w = do
   (base1 :/ d1) <- readSlice "./test/data/compare1/left"
   (base2 :/ d2) <- readSlice "./test/data/compare1/right"
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
  (base1 :/ d1) <- readSlice "./test/data/double/backup/2018-02-03T00-00-00Z.yaba-slice"
  (base2 :/ d2) <- readSlice "./test/data/double/backup/2018-08-29T07-13-19Z.yaba-slice"
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
  lodreeBackupAll <- readBackupDir stdOutLoggingEventHanler "./test/data/case4/backup/data" "./test/data/case4/backup/index"
  let lodreeBackupCurrent = currentLodree lodreeBackupAll
  dump lodreeBackupCurrent
  putStrLn  " ============ SOURCE lodreeSourceAllNodes"
  lodreeSourceOneNode <- readSourceTree stdOutLoggingEventHanler [] "./test/data/case4/source-of-maintree"
  let lodreeSourceAllNodes = makeLDir [("maintree", lodreeSourceOneNode)]
  dump lodreeSourceAllNodes
  putStrLn  " ============ COMPARE"
  let diff = compareTrees lodreeBackupCurrent lodreeSourceAllNodes
  dump (fromJust diff)


  putStrLn  " ============ HASHPAIRS - logical - lodreeBackupAll"
  dump $ createMapOfHashes lodreeBackupAll

  putStrLn $ unlines $ dirTreeToStringList (Just . toDumpS) $
    fromJust $ buildBackup lodreeBackupAll lodreeSourceAllNodes "POKUSNYBEKUP"

ee = do
  putStrLn  " ====NOVE=========================="
  lodree1 <- readSourceTree stdOutLoggingEventHanler [] "./test/data/case3/source-of-maintree"
  dump lodree1
  --putStrLn  " ====STARE=========================="
  --lodree2 <- readSourceTree' "./test/data/case3/source-of-maintree"
  --dump lodree2


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



check  :: (FromJSON a, ToJSON a, Generic a, Eq a) => Tree a -> Bool
check t = let (Right tt) = (decodeEither' . encode) t
   in tt == t


xx :: Tree String
xx = MDir $ M.fromList [
   ("jedna", MFile "1"),
   ("dva", MFile "2"),
   ("TRI", MDir (
     M.fromList [
       ("příliš žluťoučky kůň úpěl ďábelské ódy \"\\:/=[]{}() # blb", MFile "11"),
       ("ddva", MFile "22")
      ]
    )
   )
 ]

xxx :: Tree Finfo
xxx = MDir $ M.fromList [
   ("jedna", MFile $ Finfo 1 "ha"),
   ("dva", MFile $ Finfo 2 "br"),
   ("TRI", MDir (
     M.fromList [
       ("příliš žluťoučky kůň úpěl ďábelské ódy \"\\:/=[]{}() # blb", MFile $ Finfo 11 "hajal"),
       ("ddva", MFile $ Finfo 3 "sul")
      ]
    )
   )
  ]


cc = do
  B8.putStrLn $ encode xx
  B8.putStrLn $ encode xxx
  print $ check xx
  print $ check xxx
  let (Right tt :: Either ParseException (Tree Finfo)) = (decodeEither' . encode) xxx
  B8.putStrLn $ encode tt


yy = do
   print $ True == makeFilterFce []  ["ahoj"]
   print $ False == makeFilterFce ["e=ahoj"]  ["ahoj"]
   print $ False == makeFilterFce ["e~ahoj.*"]  ["ahojek"]
   print $ True == makeFilterFce ["e~ahoj.*"]  ["ahajek"]


readUserName :: EitherT Int IO String
readUserName = EitherT $ do
 str <- getLine
 if length str > 1
   then return $ Right str
   else return $ Left 111

readEmail :: EitherT Int IO String
readEmail = EitherT $ do
  str <- getLine
  if length str > 1
    then return $ Right str
    else return $ Left 222

readPassword :: IO (Either Int String)
readPassword = do
  str <- getLine
  if length str > 1
    then return $ Right str
    else return $ Left 333

login u e p = print (u,e,p)
t = do
  maybeCreds <- runEitherT $ do
    usr <- readUserName
    email <- readEmail
    pass <- EitherT readPassword
    return (usr, email, pass)
  case maybeCreds of
    Left x          -> print x
    Right (u, e, p) -> login u e p

tt = do
    putStrLn "------------------------------------------------"
    x  <- readConfig "test/data/jenconfig"
    putStrLn "-----------------HOTOVO-----------------------"
    print x


l = do

  putStrLn "------------------------------------------------"
  let x = 1.14907259 :: Double
  lo "kolo"
  lo $ printf "%.2f"  x

  -- loga printf "ah %d oj" 5
  -- putStrLn .  printf "ah %d oj" 5
  -- putStrLn ss
