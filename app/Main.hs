module Main where

import           Data.Maybe
import           GHC.IO.Encoding
--import           Filesystem.Path
import           Backup
import           Debug
import           Lib
import           LogicalDirTree
import           System.Directory.Tree
import           System.FilePath.Find
import           TreeComparator
import           TurboWare
import           YabaDirTree           hiding (RegularFile)

--import           Data.Time.Calendar
import           Data.Time.Clock

--main :: IO ()
--main = do
--  setLocaleEncoding utf8
--  getLocaleEncoding >>= print
--  now <- getCurrentTime
--  print now
  -- putStrLn "→"

import           Data.Semigroup        ((<>))
import           Options.Applicative

data Cmdline = Cmdline
  { backupDir  :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

cmdline :: Parser Cmdline
cmdline = Cmdline
      <$> strOption
          ( long "backup"
         <> metavar "BACKUP-DIR"
         <> help "Destination append only directory with backed up data, contains the \"sources.yaml\" file" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )

main :: IO ()
main = doBackup =<< execParser opts
 where
   opts = info (cmdline <**> helper)
     ( fullDesc
    <> progDesc "backing up structure without change and duplicity"
    <> header "yaba - yeat another backup" )

doBackup :: Cmdline -> IO ()
doBackup (Cmdline backupDir False n) = do
  putStrLn $ "Backing up to \"" ++ backupDir ++ "\" using definition in \"sources.yaba\""
  let sourceOfMainTree = "./test/data/case3/source-of-maintree"
  backup backupDir [("maintree", sourceOfMainTree)]
  putStrLn $ " Back up finished "
doBackup _  = return ()

-- putStrLn $ "Backup, " ++ h ++ replicate n '!'
