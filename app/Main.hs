{-# LANGUAGE TupleSections #-}

module Main where

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


-- putStrLn "→"

data Cmdline = Cmdline
  { dir        :: String
  , version    :: Bool
  , optCommand :: Command
  } deriving Show

data Command
  = Backup
  | Restore {
        destDir   :: String,
        fromDir   :: String,     -- relative dir
        checkDest :: Bool
      }
  | MarkDuplicities {
        markedDir :: String
      }
 deriving (Show)


yabaProgramInstallDir = "This value direct to use yaba isntall directory"

backupOptions :: Parser Command
backupOptions = pure Backup

restoreOptions :: Parser Command
restoreOptions = Restore
    <$> strOption
        ( long "dest-dir"
       <> metavar "DEST-DIR"
       <> help "Destination directory for restoring data. It must exists and must be empty."
        )
    <*> strOption
        ( long "from-dir"
       <> metavar "FROM-DIR"
       <> help "Subdirectory which you want to restore. (Default is the whole forest.)"
       <> value ""
        )
    <*> switch
        ( long "check-dest"
       <> help "Not to check existence and emptiness of the dest directory."
        )

markDuplicitiesOptions = MarkDuplicities
    <$> strOption
        ( long "marked-dir"
       <> metavar "MARKED-DIR"
       <> help "Directory in which the duplicities will be marked by renaming files allready backed up."
        )


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
          ( long "version"
         <> help "Display version" )
      <*> hsubparser
          (  command "backup" (info backupOptions ( progDesc "Backup your system" ))
          <> command "restore" (info restoreOptions ( progDesc "Restore backed up files" ))
          <> command "find-duuplicities" (info markDuplicitiesOptions ( progDesc "Find duplicities in another directory" ))
          )

main = do
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
doBackup x = do
  print x
  return ExitSuccess

-- putStrLn $ "Backup, " ++ h ++ replicate n '!'
