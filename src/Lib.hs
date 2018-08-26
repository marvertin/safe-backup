module Lib
    (
      HasFileName(..),
      mapTree,
      namesToPath,
      deAnchore,
      parseForestDef,

    ) where

import           Data.Either
import           Data.Function
import           Data.List             (intercalate, nub, sortBy)
import           Data.List.Unique
import           Data.Maybe
import           System.Directory.Tree
import           Text.Printf           (printf)
import           TurboWare


class  HasFileName a  where
  fileNamex :: a -> FileName


instance HasFileName (DirTree a) where
  fileNamex (Dir name _)    = name
  fileNamex (File name _)   = name
  fileNamex (Failed name _) = name

mapTree :: ([FileName] -> a -> b) -> DirTree a -> DirTree b
mapTree  =
  let
    mapa :: [FileName] -> ([FileName] -> a -> b) -> DirTree a -> DirTree b
    mapa path fun (File name x)   = File name (fun (name:path) x)
    mapa path fun (Dir name list) = Dir name $  map (mapa (name:path) fun) list
  in mapa []

namesToPath :: [FileName] -> FilePath
namesToPath list = "/" ++ intercalate "/" (reverse list)

deAnchore :: AnchoredDirTree a -> DirTree a
deAnchore (_ :/ dirtree) = dirtree

removeFirstChar :: Char -> String -> String
removeFirstChar _ [] = []
removeFirstChar c q@(x: xs)
  | x == c = xs
  | otherwise = q

splitByChar :: Char -> String -> (String, String)
splitByChar z xs = let (s1, s2) = span (/=z) xs
                   in (trim s1, trim . removeFirstChar z $ s2)

parseForestDef :: String ->  Either String [(String, String)]
parseForestDef s =
    (sequence $ mapMaybe check . zip [1..] . map (splitByChar '=') . lines $ s)
      >>= checkDupl
  where
     check :: (Int, (String, String)) -> Maybe (Either String (String, String))
     check (_, ([], [])) = Nothing
     check (lineNum, ([], _)) = Just $ Left $ "Missing direcotry name (right part) at line " ++ show lineNum
     check (lineNum, (_, [])) = Just $ Left $ "Missing tree name (let part) at line " ++ show lineNum
     check (_, q)  = Just $ Right q

     checkDupl :: [(String, String)]  -> Either String [(String, String)]
     checkDupl forest = let dupl = repeated $ map fst forest
                        in if null dupl then Right forest
                                        else Left $ "duplicated tree names: " ++ show dupl
