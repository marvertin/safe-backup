module Forest
    (
      parseForestDef,
      readForestDef,

    ) where

import           Control.Exception
import           Data.Bifunctor
import           Data.Either
import           Data.Function
import           Data.List             (intercalate, nub, sortBy)
import           Data.List.Unique
import           Data.Maybe
import           Lib
import           System.Directory.Tree
import           TurboWare
import           Types


type ForestDef = [(String, String)]

forestDefName :: FileName
forestDefName = "yaba-forest-definition.properties"

parseForestDef :: String ->  Either ErrMsg ForestDef
parseForestDef forestDefStr =
    sequence (mapMaybe check . zip [1..] . map (splitByChar '=') . lines $ forestDefStr)
      >>= checkDupl
  where
     check :: (Int, (String, String)) -> Maybe (Either String (String, String))
     check (_, ([], [])) = Nothing
     check (lineNum, ([], _)) = Just $ Left $ "Missing direcotry name (right part) at line " ++ show lineNum
     check (lineNum, (_, [])) = Just $ Left $ "Missing tree name (left part) at line " ++ show lineNum
     check (_, q)  = Just $ Right q

     checkDupl :: ForestDef  -> Either ErrMsg ForestDef
     checkDupl forest = let dupl = repeated $ map fst forest
                        in if null dupl then Right forest
                                        else Left $ "duplicated tree names: " ++ show dupl

readForestDef :: FilePath -> IO (Either ErrMsg ForestDef)
readForestDef backupDir =
  let forestFile = backupDir ++ "/" ++ forestDefName
  in   catch (bimap ((forestFile ++ ": ") ++) id . parseForestDef <$> readFile forestFile)
         (\e -> return $ Left $ show (e :: IOException))
