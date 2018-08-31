{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module SliceMerger (
  mergeToLodree,
  mergesToLodree,
) where

import qualified Crypto.Hash.SHA1      as Cr

import           Control.Applicative
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.UTF8  as BSU
import           Data.Function
import           Data.List
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Yaml
import           GHC.Generics
import           Lib
import           Lodree
import           System.Directory.Tree (DirTree (Dir, File), FileName)
import           System.FilePath
import           TurboWare
import           Types
import           YabaDirTree
import           YabaFileContent



mergesToLodree :: Lodree -> [YabaDirTree] -> Lodree
mergesToLodree = foldl mergeToLodree

mergeToLodree :: Lodree -> YabaDirTree -> Lodree
mergeToLodree lodree sliceTree =
   let mergeSliceTo = flip merge1 sliceTree -- functio
   -- must be 2 times applied due to resolve duplicities added in previews
   -- slice where are physical links to the same slice, thand droped the last but one
   in (dropPenuliamate . mergeSliceTo . mergeSliceTo) lodree
 where dropPenuliamate (LDir _ (last' : _ : rest)) = makeLDir (last': rest)

merge1 :: Lodree -> YabaDirTree -> Lodree
merge1 rootLodree rootDirTree = let
--     x = 0
       rootList (LDir _ list) =  list
       newLodree = fromMaybe emptyLodree (merge' (Just $ currentLodree rootLodree) (Just rootDirTree))
    in  makeLDir ((fileNamex rootDirTree, newLodree) : rootList rootLodree)
    --in  LDir (DRee 0 0 Strict.empty) [ (rootList rootLodree)
  where
    merge' :: Maybe Lodree -> Maybe YabaDirTree -> Maybe Lodree
    merge' Nothing Nothing = Nothing
    merge' Nothing dirtree = merge' (Just emptyLodree) dirtree -- nemámeli složku, stvoříme ji
    merge' lodree Nothing = lodree
    merge' _ (Just (File _ regfile@ RegularFile{}))= Just $ LFile (fordInfo2Ree regfile)
    merge' lodree (Just (File _ (YabaFile content)))
      | isDelete content = Nothing
      | isLogicalLink   content = findTarget content (currentLodree rootLodree) <|> lodree
      | isPhysicalLink  content = findTarget content rootLodree <|> lodree
    merge' (Just (LDir ree subdirs)) (Just (Dir name dirtrees)) =
       let pa = pairDirs subdirs (filterOutYaba dirtrees)
           qa = map (\(name, lodree, dirtree) ->  (name, merge' lodree dirtree)) pa
           ra :: [(FileName, Lodree)]
           ra = mapMaybe tupleMaybeUpSnd qa
       in if null ra then Nothing -- we dotnt want empty dirs
                     else Just (makeLDir ra)

------------------------------------ private -------------------------
pairDirs :: [(FileName, Lodree)] -> [DirTree FordInfo]
             -> [(FileName, Maybe Lodree, Maybe (DirTree FordInfo))]
pairDirs lodree2 dirtree =
    let preZiped = zipMaybe fst pickPureFordName lodree2 dirtree
    in map (\(name, lodree, dirtree) -> (name, snd <$> lodree, dirtree)) preZiped

filterOutYaba :: [DirTree FordInfo] -> [DirTree FordInfo]
filterOutYaba fulllist = let
    list = filter (isJust . extractPureFordName . fileNamex ) fulllist -- jen správně udělaná yaba fajly
    (yabaall, regular) = partition isYabaFile list
    regularFordNames = fileNamex <$> regular
    yabaNoHiden = filter (not . (`elem` regularFordNames) . pickPureFordName) yabaall -- regular file must hide yabas
    yabas = nubBy ((==) `on` pickPureFordName) yabaNoHiden
  in yabas ++ regular


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
fordInfo2Ree (RegularFile a b c) = Ree { rphysPath = a, rcount = 1, rsize = b, rhash = c }

isDelete :: JabaContent -> Bool
isDelete = isYabaRemove . parseYabaFile

isLink :: JabaContent -> Bool
isLink = isYabaLink . parseYabaFile;

isLogicalLink :: JabaContent -> Bool
isLogicalLink = isYabaLogicalLink . parseYabaFile;

isPhysicalLink :: JabaContent -> Bool
isPhysicalLink = isYabaPhysicalLink . parseYabaFile;

findTarget :: JabaContent -> Lodree -> Maybe Lodree
findTarget content = findLodreeNode $ (getLinkTarget . parseYabaFile) content
