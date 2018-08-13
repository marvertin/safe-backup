{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module LogicalDirTree (
  Lodree(..),
  Ree(..),
  DRee(..),
  emptyLodree,
  hashLodree,
  findLodreeNode,

  mergeToLodree,
  currentLodree,
) where

import qualified Crypto.Hash.SHA1      as Cr

import qualified Data.ByteString       as Strict
import qualified Data.ByteString.UTF8  as BSU
import           Data.Function
import           Data.List
import           Data.Maybe
import           Lib
import           System.Directory.Tree (DirTree (Dir, File), FileName)
import           System.FilePath
import           TurboWare
import           Types
import           YabaDirTree
import           YabaFileContent

-- data LfInfo = LfInfo { physPath :: FilePath}
data Ree = Ree { physPathx :: FilePath, size :: FileSize, hash :: Hash } deriving (Show)
data DRee = DRee { dsize :: FileSize, dcount :: Int, dhash :: Hash } deriving (Show)

data Lodree = LFile Ree
            | LDir DRee [(FileName, Lodree)]
            deriving (Show)

emptyLodree :: Lodree
emptyLodree = LDir (DRee 0 0 Strict.empty) []

findLodreeNode = findNode

mergeToLodree = merge

merge :: Lodree -> DirTree FordInfo -> Lodree
merge rootLodree rootDirTree = let
--     x = 0
       rootList (LDir _ list) =  list
       newLodree = fromMaybe emptyLodree (merge' (Just $ currentLodree rootLodree) (Just rootDirTree))
    in  LDir (DRee 0 0 Strict.empty)  ((fileNamex rootDirTree, newLodree) : rootList rootLodree)
    --in  LDir (DRee 0 0 Strict.empty) [ (rootList rootLodree)
  where
    merge' :: Maybe Lodree -> Maybe (DirTree FordInfo) -> Maybe Lodree
    merge' Nothing Nothing = Nothing
    merge' Nothing dirtree = merge' (Just emptyLodree) dirtree -- nemámeli složku, stvoříme ji
    merge' lodree Nothing = lodree
    merge' _ (Just (File _ regfile@ RegularFile{}))= Just $ LFile (fordInfo2Ree regfile)
    merge' _ (Just (File _ (YabaFile content)))
      | isDelete content = Nothing
      | isLogicalLink   content = findTarget content (currentLodree rootLodree)
      | isPhysicalLink  content = findTarget content rootLodree
    merge' (Just (LDir ree subdirs)) (Just (Dir name dirtrees)) =
       let pa = pairDirs subdirs (filterOutYaba dirtrees)
           qa = map (\(name, lodree, dirtree) ->  (name, merge' lodree dirtree)) pa
           ra :: [(FileName, Lodree)]
           ra = mapMaybe tupleMaybeUpSnd qa
       in if null ra then Nothing -- we dotnt want empty dirs
                     else Just (LDir (foldToDree ra) ra)

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

foldToDree :: [(FileName, Lodree)] -> DRee
foldToDree list = let
    sortedList = sortBy (compare `on` fst) list
    names = map (BSU.fromString . fst) list
    hashes = map (pickHash . snd) sortedList
  in DRee { dsize = sum $ (pickSize . snd) <$> list,
            dcount = sum $ (pickCount . snd) <$> list,
            dhash = Cr.finalize $ foldl Cr.update Cr.init (hashes ++ names)
          }

pickSize :: Lodree -> FileSize
pickSize (LFile ree)   = size ree
pickSize (LDir dree _) = dsize dree

pickHash :: Lodree -> Hash
pickHash (LFile ree)   = hash ree
pickHash (LDir dree _) = dhash dree

hashLodree = pickHash

pickCount :: Lodree -> Int
pickCount (LFile _)     = 1
pickCount (LDir dree _) = dcount dree


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
        todump (filename, q@(LDir dree _)) =   ("/" ++ filename ++ " " ++ printDRee dree) : toDump q

printRee :: Ree ->  String
printRee Ree {..} = "  #" ++ show size ++ " " ++ toHexStr hash ++ " \"" ++ physPathx ++ "\""

printDRee :: DRee ->  String
printDRee DRee {..} = "  #" ++ show dcount  ++ "/" ++ show dsize ++ "  " ++ toHexStr dhash

--w = do
--  putStrLn $ unlines $ lodreeToStringList (gen "abcd")
