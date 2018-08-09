{-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE RecordWildCards #-}

module LogicalDirTree (
  merge,
  gen,
  lodreeToStringList,
  extractPureFordName,
  w
) where

import           Data.Function
import           Data.List
import           Data.Maybe
import           Lib
import           System.Directory.Tree (DirTree (Dir, File), FileName)
import           System.FilePath
import           Types
import           YabaDirTree

data Ree = Ree { physPathx :: FilePath, size :: FileSize, hash :: Hash } deriving (Show)
data Lodree = LFile ()
            | LDir () [(FileName, Lodree)]
            deriving (Show)


merge :: Maybe Lodree -> Maybe (DirTree FordInfo) -> Maybe Lodree
merge Nothing Nothing = Nothing
merge Nothing dirtree = merge (Just (LDir () [])) dirtree -- nemámeli složku, stvoříme ji
merge lodree Nothing = lodree
merge _ (Just (File _ regfile@ RegularFile{}))= Just $ LFile () -- (cnvt regfile)
merge _ (Just (File _ (YabaFile content)))
  | isDelete content = Nothing
  | isLink   content = findTarget content
merge (Just (LDir ree subdirs)) (Just (Dir name dirtrees)) =
   let pa = pairDirs subdirs (filterYaba dirtrees)
       qa = map (\(name, lodree, dirtree) ->  (name, merge lodree dirtree)) pa
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

cnvt :: FordInfo -> Ree
cnvt (RegularFile a b c) = Ree { physPathx = a, size = b, hash = c }

isDelete :: JabaContent -> Bool
isDelete _  = True

isLink :: JabaContent -> Bool
isLink _  = False

findTarget :: JabaContent -> Maybe Lodree
findTarget _ = Nothing

lodreeToStringList :: Lodree -> [String]
lodreeToStringList (LFile ()) = ["<file>"]
lodreeToStringList (LDir () items) = ("    " ++) <$> (items >>= todump)
   where
      todump :: (FileName, Lodree) -> [String]
      todump (filename, q@(LFile _)) = prependToFirst (filename ++ ": ") (lodreeToStringList q)
      todump (filename, q@(LDir _ _)) =   ("/" ++ filename) : lodreeToStringList q

gen :: String -> Lodree
gen ""         = LFile ()
gen [_]        = LFile ()
gen zz@(_: zs) = LDir () [(zz ++ "1", gen zs), (zz ++ "2", gen zs), (zz ++ "9", gen (tail zs))]

prependToFirst :: [a] -> [[a]] -> [[a]]
prependToFirst [] []     = []
prependToFirst x []      = [x]
prependToFirst x (y: ys) = (x ++ y) : ys


w = do
  putStrLn $ unlines $ lodreeToStringList (gen "abcd")
