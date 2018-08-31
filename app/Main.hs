{-# LANGUAGE TupleSections #-}

module Main where

import           Data.Maybe
import           GHC.IO.Encoding
--import           Filesystem.Path
import           Backup
import           Debug
import           Lib
import           SliceMerger
import           System.Directory.Tree
import           System.FilePath.Find
import           TreeComparator
import           TurboWare

--import           Data.Time.Calendar
import           Config
import           Control.Monad
import           Data.Time.Clock
import           Data.Version          (showVersion)
import qualified Paths_yaba            (version)
import           Slice
import           System.Directory
import           System.Environment
import           System.TimeIt
import           Tree
import           Types

  -- putStrLn "→"

import           Data.Semigroup        ((<>))
import           Options.Applicative
import           System.Exit

data Cmdline = Cmdline
  { backupDir  :: String
  , version    :: Bool
  , quiet      :: Bool
  , enthusiasm :: Int }

cmdline :: Parser Cmdline
cmdline = Cmdline
      <$> strOption
          ( long "backup"
         <> metavar "BACKUP-DIR"
         <> help "Destination append only directory with backed up data, contains the \"sources.yaml\" file" )
      <*> switch
          ( long "version"
         <> help "Display version" )
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

--  test directory: ./test/data/case3/backup
main = do
  setLocaleEncoding utf8
  putStrLn $ "yaba " ++ showVersion Paths_yaba.version ++ " - yeat another backup"
  startTime <- getCurrentTime
  -- getLocaleEncoding >>= print
  -- now <- getCurrentTime
  -- print now
  exitCode <- timeIt  main'
  endTime <- getCurrentTime
  putStrLn $ "Total time: " ++ show (diffUTCTime endTime startTime)
  exitWith exitCode

main' :: IO ExitCode
main' = doBackup =<< execParser opts
 where
   opts = info (cmdline <**> helper)
     ( fullDesc
    <> progDesc "backing up structure without change and duplicity"
    <> header "yaba - yeat another backup" )


doBackup :: Cmdline -> IO ExitCode
doBackup (Cmdline _ True _ _) = do
  putStrLn $ "yaba " ++ showVersion Paths_yaba.version ++ " - yeat another backup"
  return ExitSuccess
doBackup (Cmdline backupDir _ False n) = do
  backupDirAbs <- makeAbsolute backupDir
  putStrLn $ "Backing up to \"" ++ backupDirAbs ++ "\" using definition in \"" ++ configFileName ++ "\""
  config <- readConfig backupDirAbs
  let forestDef = pickForestDef <$> config
  case forestDef of
    Left msg -> do
      putStrLn msg
      return $ ExitFailure 1
    Right forest -> do
      results <- backup backupDirAbs forest
      let failus = fmap (\(b :/ d) -> (b, failures d)) results
      let failus2 = failus >>= \(b, list)  -> (b,) <$> list
      if null failus2 then do
                          putStrLn "**** SUCCESS **** - backup has finished"
                          return ExitSuccess
                      else do
                          putStrLn "!!!!!!!!!!!!!!!!!!! ERROR LIST !!!!!!!!!!!!"
                          forM_ failus (\(b, list) -> do
                              putStrLn b
                              forM_ list (\x -> do
                                putStr "    "
                                print x
                               )
                            )
                          putStrLn $ "!!!!!!!!!!!! " ++ show (length failus2) ++ " ERRORS !!!!!!!!!"
                          return $ ExitFailure 2


  --let sourceOfMainTree = "./test/data/case3/source-of-maintree"
  -- backup backupDir [("maintree", sourceOfMainTree)]
  --putStrLn $ " Back up finished "
doBackup _  = return (ExitFailure 5)

-- putStrLn $ "Backup, " ++ h ++ replicate n '!'
