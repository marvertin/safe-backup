{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Monad
import           Data.Either
import           Data.Maybe
import           Data.Semigroup        ((<>))
import           Data.Time.Clock
import           Data.Version          (showVersion)
import           GHC.IO.Encoding
import           Options.Applicative
import qualified Paths_yaba            (version)
import           System.Directory
import           System.Directory.Tree
import           System.Environment
import           System.Exit
import           System.FilePath.Find
import           System.IO
import           System.TimeIt
import           Text.Printf

import           Backup
import           Config
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
  { backupDir  :: String
  , version    :: Bool
  , quiet      :: Bool
  , enthusiasm :: Int
  , scan       :: Bool
  }

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
      <*> switch
         ( long "scan"
         <> help "Jen skenovat, testovací účel" )

--  test directory: ./test/data/case3/backup
main = do
  setLocaleEncoding utf8

  hSetBuffering stdout LineBuffering
  hGetBuffering stdout >>= print
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
doBackup (Cmdline dirToScan _ _ _ True) = do
  putStrLn $ "Budeme jen skenovat adrear: " ++ dirToScan
  scanDirectoryTest dirToScan
  return $ ExitFailure 8
doBackup (Cmdline _ True _ _ False) = do
  putStrLn $ "yaba " ++ showVersion Paths_yaba.version ++ " - yeat another backup"
  return ExitSuccess
doBackup (Cmdline backupDir _ False n _) = do
  backupDirAbs <- makeAbsolute backupDir
  putStrLn $ "Backing up to \"" ++ backupDirAbs ++ "\" using definition in \"" ++ configFileName ++ "\""
  maybeForestDef <- readConfig backupDirAbs
  case maybeForestDef of
    Nothing -> do
      putStrLn "!!! ERRORS starting backup !!!"
      return $ ExitFailure 1
    Just (Cfg sliceNameStrategy forest empties) -> do
      results <- backup backupDirAbs forest
      let failus = fmap (\(b :/ d) -> (b, failures d)) results
      let failus2 = failus >>= \(b, list)  -> (b,) <$> list
      if null failus2 then do
                          if null empties then do
                             putStrLn "**** SUCCESS **** - backup has finished"
                             return ExitSuccess
                            else do
                               putStrLn $ "**** success, BUT some trees are empty or unaccessible: " ++ (show empties)
                               return $ ExitFailure 7
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
