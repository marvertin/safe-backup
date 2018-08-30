{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module LogicalDirTree (
  Lodree(..),
  Ree(..),
  emptyLodree,
  hashLodree,
  makeLDir,
  findLodreeNode,

  mergeToLodree,
  mergesToLodree,
  currentLodree,
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
import           System.Directory.Tree (DirTree (Dir, File), FileName)
import           System.FilePath
import           TurboWare
import           Types
import           YabaDirTree
import           YabaFileContent

-- data LfInfo = LfInfo { physPath :: FilePath}
data Ree = Ree { rphysPath :: FilePath, rcount :: Int, rsize :: FileSize, rhash :: Hash } deriving (Show)

data Lodree = LFile Ree
            | LDir Ree [(FileName, Lodree)]
            deriving (Show)

emptyLodree :: Lodree
emptyLodree = makeLDir []

makeLDir :: [(FileName, Lodree)] -> Lodree
makeLDir list = LDir (foldToDree list) list
  where
    foldToDree :: [(FileName, Lodree)] -> Ree
    foldToDree list = let
        sortedList = sortBy (compare `on` fst) list
        names = map (BSU.fromString . fst) list
        hashes = map (pickHash . snd) sortedList
      in Ree { rphysPath = "",
               rsize = sum $ (pickSize . snd) <$> list,
               rcount = sum $ (pickCount . snd) <$> list,
               rhash = Cr.finalize $ foldl Cr.update Cr.init (hashes ++ names)
            }-- emptyDRee = DRee 0 0 Strict.empty

findLodreeNode = findNode

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

currentLodree :: Lodree -> Lodree
currentLodree (LDir _ [])                 = emptyLodree
currentLodree (LDir _ ((_, current) : _)) = current
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


ree :: Lodree -> Ree
ree (LFile ree)  = ree
ree (LDir ree _) = ree

pickSize :: Lodree -> FileSize
pickSize = rsize . ree

pickHash :: Lodree -> Hash
pickHash = rhash . ree

hashLodree = pickHash

pickCount :: Lodree -> Int
pickCount = rcount . ree


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
findTarget content = findNode $ (getLinkTarget . parseYabaFile) content

findNode :: FilePath -> Lodree -> Maybe Lodree
findNode [] lodree = Just lodree
findNode "/" lodree = Just lodree
findNode path (LFile _) = error $ "Uz mame soubor, ale v ceste jeste je: " ++ path
findNode path (LDir _ list) = let
  (name, rest) = break ('/'==) (dropPrefixSlashes path)
  lodree2 = snd <$> find ((name==) . fst) list
  in lodree2 >>= findNode rest

--------------------------------------------------------
--
-- Instances for YAML
instance ToJSON Ree where
  -- toJSON (Finfo x y) = object ["x" .= x, "y" .= y]
  toJSON Ree{..} = let val = show rsize ++ " " ++ toHexStr rhash
     in toJSON val

instance ToJSON Lodree where
   toJSON (LFile ree)   = toJSON ree
   toJSON (LDir _ list) = toJSON (M.fromList list)

--------------------------------------------------------
-- The rest of this modul is for DEBUGING purpose only - it is dump
--
instance Dumpable Lodree where
  -- toDump :: DirCompare -> [String]

  toDump :: Lodree -> [String]
  toDump (LFile ree) = [printRee ree]
  toDump (LDir _ items) = ("    " ++) <$> (items >>= todump)
     where
        todump :: (FileName, Lodree) -> [String]
        todump (filename, q@(LFile _)) = prependToFirst (filename ++ ": ") (toDump q)
        todump (filename, q@(LDir ree _)) =   ("/" ++ filename ++ " " ++ printRee ree) : toDump q

printRee :: Ree ->  String
printRee Ree {..} = "  #" ++ show rsize ++ " " ++ toHexStr rhash ++ " \"" ++ rphysPath ++ "\""


--w = do
--  putStrLn $ unlines $ lodreeToStringList (gen "abcd")
