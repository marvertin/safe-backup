{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module LogicalDirTree (
  Lodree,
  emptyLodree,
  merge,

  lodreeToStringList,

  -- dočasné kvůli ladění
  extractPureFordName,
  findNode,
  gen,

) where

import qualified Data.ByteString       as Strict
import           Data.Function
import           Data.List
import           Data.Maybe
import           Lib
import           System.Directory.Tree (DirTree (Dir, File), FileName)
import           System.FilePath
import           Types
import           YabaDirTree
import           YabaFileContent


-- data LfInfo = LfInfo { physPath :: FilePath}
data Ree = Ree { physPathx :: FilePath, size :: FileSize, hash :: Hash } deriving (Show)
data Lodree = LFile Ree
            | LDir () [(FileName, Lodree)]
            deriving (Show)

emptyLodree = LDir () []

merge :: Lodree -> DirTree FordInfo -> Lodree
merge rootLodree rootDirTree = fromMaybe emptyLodree (merge' (Just rootLodree) (Just rootDirTree))
  where
    merge' :: Maybe Lodree -> Maybe (DirTree FordInfo) -> Maybe Lodree
    merge' Nothing Nothing = Nothing
    merge' Nothing dirtree = merge' (Just emptyLodree) dirtree -- nemámeli složku, stvoříme ji
    merge' lodree Nothing = lodree
    merge' _ (Just (File _ regfile@ RegularFile{}))= Just $ LFile (fordInfo2Ree regfile)
    merge' _ (Just (File _ (YabaFile content)))
      | isDelete content = Nothing
      | isLink   content = findTarget content rootLodree
    merge' (Just (LDir ree subdirs)) (Just (Dir name dirtrees)) =
       let pa = pairDirs subdirs (filterYaba dirtrees)
           qa = map (\(name, lodree, dirtree) ->  (name, merge' lodree dirtree)) pa
           ra = mapMaybe tupleMaybeUpSnd qa
       in if null ra then Nothing
                     else Just (LDir () ra) -- we dotnt want empty dirs

pairDirs :: [(FileName, Lodree)] -> [DirTree FordInfo]
             -> [(FileName, Maybe Lodree, Maybe (DirTree FordInfo))]
pairDirs lodree2 dirtree =
    let preZiped = zipMaybe fst pickPureFordName lodree2 dirtree
    in map (\(name, lodree, dirtree) -> (name, snd <$> lodree, dirtree)) preZiped

filterYaba :: [DirTree FordInfo] -> [DirTree FordInfo]
filterYaba fulllist = let list = filter (isJust . extractPureFordName . fileNamex ) fulllist -- jen správně udělaná yaba fajly
                          (yabaall, regular) = partition isYabaFile list
                          regularFordNames = fileNamex <$> regular
                          yaba = filter (not . (`elem` regularFordNames) . pickPureFordName) yabaall -- regular must file hide yabas
                          yabax = nubBy ((==) `on` pickPureFordName) yaba
     in yabax ++ regular

{- |
  extract pure filename from yaba files
    "ahoj.txt" -> "ahoj.txt"
    "~COKoLI~normalni jmen~o.yaba" -> Just "normalni jmen~o"
    "~COkOLI~normalni jmen~o.xaba" -> Just "~COkOLI~normalni jmen~o.xaba"
    "anyother patter.yaba" -> Nothing
-}
extractPureFordName :: FileName -> Maybe FileName
extractPureFordName [] = Nothing
extractPureFordName fullName
  | not $ isExtensionOf yabaSuffix fullName = Just fullName
extractPureFordName ('~' : zbytek) =
   let pureName = dropWhile ('~' ==) . dropWhile ('~' /=) $ takeBaseName zbytek
   in if null pureName then Nothing
                       else Just pureName
extractPureFordName _ = Nothing

pickPureFordName :: DirTree a -> FileName
pickPureFordName = fromJust . extractPureFordName . fileNamex  -- function returning name in yaba and no yaba files

fordInfo2Ree :: FordInfo -> Ree
fordInfo2Ree (RegularFile a b c) = Ree { physPathx = a, size = b, hash = c }

isDelete :: JabaContent -> Bool
isDelete = isYabaRemove . parseYabaFile

isLink :: JabaContent -> Bool
isLink = isYabaLink . parseYabaFile

findTarget :: JabaContent -> Lodree -> Maybe Lodree
findTarget content = findNode $ (getLinkTarget . parseYabaFile) content

dropStartSlash :: FilePath -> FilePath
dropStartSlash []          = []
dropStartSlash ('/': rest) = rest
dropStartSlash path        = path

findNode :: FilePath -> Lodree -> Maybe Lodree
findNode [] lodree = Just lodree
findNode "/" lodree = Just lodree
findNode path (LFile _) = error $ "Uz mame soubor, ale v ceste jeste je: " ++ path
findNode path (LDir () list) = let
  (name, rest) = break ('/'==) (dropStartSlash path)
  lodree2 = snd <$> find ((name==) . fst) list
  in lodree2 >>= findNode rest



lodreeToStringList :: Lodree -> [String]
lodreeToStringList (LFile ree) = [printRee ree]
lodreeToStringList (LDir () items) = ("    " ++) <$> (items >>= todump)
   where
      todump :: (FileName, Lodree) -> [String]
      todump (filename, q@(LFile _)) = prependToFirst (filename ++ ": ") (lodreeToStringList q)
      todump (filename, q@(LDir _ _)) =   ("/" ++ filename) : lodreeToStringList q

gen :: String -> Lodree
gen ""         = LFile (Ree "" 0 Strict.empty)
gen [_]        = LFile (Ree "" 0 Strict.empty)
gen zz@(_: zs) = LDir () [(zz ++ "1", gen zs), (zz ++ "2", gen zs), (zz ++ "9", gen (tail zs))]

prependToFirst :: [a] -> [[a]] -> [[a]]
prependToFirst [] []     = []
prependToFirst x []      = [x]
prependToFirst x (y: ys) = (x ++ y) : ys

printRee :: Ree ->  String
printRee Ree {..} = "  #" ++ show size ++ " " ++ toHexStr hash ++ " \"" ++ physPathx ++ "\""


--w = do
--  putStrLn $ unlines $ lodreeToStringList (gen "abcd")
