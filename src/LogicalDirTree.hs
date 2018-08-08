{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

module LogicalDirTree (

) where

import YabaDirTree
import Types
import Lib
import           System.Directory.Tree(FileName, DirTree(File, Dir))
import Data.Maybe

data Ree = Ree { physPathx :: FilePath, size :: FileSize, hash :: Hash } deriving (Show)
data Lodree = LFile Ree
            | LDir () [(FileName, Lodree)]
            deriving (Show)

merge :: Maybe Lodree -> Maybe (DirTree FordInfo) -> Maybe Lodree
merge Nothing Nothing = Nothing
merge lodree Nothing = lodree
merge _ (Just (File _ regfile@ RegularFile{}))= Just $ LFile (cnvt regfile)
merge _ (Just (File _ (YabaFile content)))
  | isDelete content = Nothing
  | isLink   content = findTarget content
merge (Just (LDir ree subdirs)) (Just (Dir name dirtrees)) =
   let pa = pairDirs subdirs dirtrees
       qa = map (\(name, lodree, dirtree) ->  (name, merge lodree dirtree)) pa
       ra = mapMaybe tupleMaybeUpSnd qa
   in if null ra then Just (LDir () ra)
                 else Nothing -- we dotnt want empty dirs
merge Nothing dirtree = merge (Just (LDir () [])) dirtree -- nemámeli složku, stvoříme ji

pairDirs :: [(fileName, Lodree)] -> [DirTree FordInfo]
             -> [(FileName, Maybe Lodree, Maybe (DirTree FordInfo))]
pairDirs _ _ = []

cnvt :: FordInfo -> Ree
cnvt (RegularFile a b c) = Ree { physPathx = a, size = b, hash = c }

isDelete :: JabaContent -> Bool
isDelete _  = True

isLink :: JabaContent -> Bool
isLink _  = False

findTarget :: JabaContent -> Maybe Lodree
findTarget _ = Nothing
