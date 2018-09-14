{-# LANGUAGE TupleSections #-}

module Main2 where

import           Control.Monad
import           Control.Monad
import           Data.Either
import           Data.Maybe
import           Data.Semigroup                ((<>))
import           Data.Time.Clock
import           Data.Version                  (showVersion)
import           GHC.IO.Encoding
import           Options.Applicative
import qualified Paths_yaba                    (version)
import           System.Directory
import           System.Directory.Tree
import           System.Environment
import qualified System.Environment.Executable as SEE
import           System.Exit
import           System.FilePath.Find
import           System.IO
import           System.TimeIt
import           Text.Printf

import           Backup
import           Config
import           Context
import           Debug
import           DirScan
import           Lib
import           Log
import           Slice
import           SliceNameStrategy
import           SliceToLodree
import           SourceTree
import           Tree
import           TreeComparator
import           TurboWare
import           Types



-- putStrLn "→"

data Cmdline = Cmdline
  { dir     :: String
  , backup  :: Bool
  , scan    :: Bool
  , version :: Bool
  }

yabaProgramInstallDir = "This value direct to use yaba isntall directory"

cmdline :: Parser Cmdline
cmdline = Cmdline
      <$> strOption
          ( long "dir"
         <> short 'd'
         <> metavar "BACKUP-DIR"
         <> help "Directory with backup, must contains \"yaba-config.yaml\" file. (default: directory where the executable of Yaba program is)"
         <> value yabaProgramInstallDir
            )
      <*> switch
          ( long "backup"
         <> help "Back up your files." )
      <*> switch
          ( long "scan"
         <> help "Only scans backup folder and create indexes" )
      <*> switch
          ( long "version"
         <> help "Display version" )

xmain = do
  setLocaleEncoding utf8
  putStrLn $ "yaba " ++ showVersion Paths_yaba.version ++ " - yeat another backup"
  exitCode <- timeIt  main'
  exitWith exitCode

main' :: IO ExitCode
main' = doBackup =<< execParser opts
 where
   opts = info (cmdline <**> helper)
     ( fullDesc
    <> progDesc "backing up structure without change and duplicity"
    <> header "yaba - yeat another backup" )


doBackup :: Cmdline -> IO ExitCode
doBackup (Cmdline _ _ _ True) = do
  putStrLn $ "yaba " ++ showVersion Paths_yaba.version ++ " - yeat another backup"
  (pathWithProgram, _) <- SEE.splitExecutablePath
  return ExitSuccess
doBackup (Cmdline enteredBackupDir True _ _) = do
  (pathWithProgram, _) <- SEE.splitExecutablePath
  backupDirAbs <- makeAbsolute $
   if enteredBackupDir == yabaProgramInstallDir then pathWithProgram
                                                else enteredBackupDir

  putStrLn $ "Backing up to \"" ++ backupDirAbs
  withContext backupDirAbs  cmdBackup
  -- (showVersion Paths_yaba.version)


  --let sourceOfMainTree = "./test/data/case3/source-of-maintree"
  -- backup backupDir [("maintree", sourceOfMainTree)]
  --putStrLn $ " Back up finished "
doBackup _  = return (ExitFailure 5)

-- putStrLn $ "Backup, " ++ h ++ replicate n '!'
