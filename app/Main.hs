{-# LANGUAGE TupleSections #-}

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
import           Config
import           Control.Monad
import           Data.Time.Clock
import           System.Directory
import           System.TimeIt
import           Types


--main :: IO ()
--main = do
--  setLocaleEncoding utf8
--  getLocaleEncoding >>= print
--  now <- getCurrentTime
--  print now
  -- putStrLn "→"

import           Data.Semigroup        ((<>))
import           Options.Applicative
import           System.Exit

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

--  test directory: ./test/data/case3/backup
main = do
  setLocaleEncoding utf8
  getLocaleEncoding >>= print
  now <- getCurrentTime
  print now
  timeIt  main'

main' :: IO ()
main' = doBackup =<< execParser opts
 where
   opts = info (cmdline <**> helper)
     ( fullDesc
    <> progDesc "backing up structure without change and duplicity"
    <> header "yaba - yeat another backup" )


doBackup :: Cmdline -> IO ()
doBackup (Cmdline backupDir False n) = do
  backupDirAbs <- makeAbsolute backupDir
  putStrLn $ "Backing up to \"" ++ backupDirAbs ++ "\" using definition in \"" ++ configFileName ++ "\""
  config <- readConfig backupDirAbs
  let forestDef = pickForestDef <$> config
  case forestDef of
    Left msg -> do
      putStrLn msg
      exitWith $ ExitFailure 1
    Right forest -> do
      results <- backup backupDirAbs forest
      let failus = fmap (\(b :/ d) -> (b, failures d)) results
      let failus2 = failus >>= \(b, list)  -> (b,) <$> list
      if null failus2 then putStrLn "**** SUCCESS **** - backup has finished"
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
                          exitWith $ ExitFailure 2


  --let sourceOfMainTree = "./test/data/case3/source-of-maintree"
  -- backup backupDir [("maintree", sourceOfMainTree)]
  --putStrLn $ " Back up finished "
doBackup _  = return ()

-- putStrLn $ "Backup, " ++ h ++ replicate n '!'
