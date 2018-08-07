module DirTreeCompare
    (
    DirTree2(LeftFile, RightFile, BothFile, Dir2),
    mergeTrees,
    leftCount,
    rightCount,
    fileName,
    ) where

import           DirTree
import           Lib
import           System.Directory.Tree (DirTree (Dir, Failed, File), FileName)
import           Text.Printf           (printf)

import           Crypto.Hash.SHA1      (hashlazy)
import           Data.Function
import           Data.List             (sortBy)


data Dvoj a = Levy (DirTree a) | Pravy (DirTree a) | Oboje (DirTree a) (DirTree a) deriving (Show, Eq, Ord)

data DirTree2 a = LeftFile FileName a | RightFile FileName a | BothFile FileName a a | Dir2 FileName [DirTree2 a] deriving (Show, Eq, Ord)

leftCount :: DirTree2 a -> Integer
leftCount LeftFile {}      = 1
leftCount BothFile {}      = 1
leftCount RightFile {}     = 0
leftCount (Dir2 _ content) = sum $ map leftCount content

rightCount :: DirTree2 a -> Integer
rightCount LeftFile {}      = 0
rightCount BothFile {}      = 1
rightCount RightFile {}     = 1
rightCount (Dir2 _ content) = sum $ map rightCount content

class  HasFileName a  where
  fileName :: a -> FileName

instance HasFileName (DirTree2 a) where
  fileName (LeftFile name _)   = name
  fileName (RightFile name _)  = name
  fileName (BothFile name _ _) = name
  fileName (Dir2 name _)       = name

instance HasFileName (DirTree a) where
  fileName (Dir name _)    = name
  fileName (File name _)   = name
  fileName (Failed name _) = name


mergeDirLists :: [DirTree a] -> [DirTree a] -> [(FileName, Dvoj a)]
mergeDirLists xs ys = sortBy (compare `on` fst) [ (xname, Oboje x y) | x <- xs, y <- ys,
                              let xname = fileName x, let yname = fileName y,
                              xname == yname ] -- it is quadratic !
                  ++  [ (xname, Levy x) | x <- xs,
                              let xname = fileName x,
                               xname `notElem` map fileName ys]
                  ++  [ (xname, Pravy y) | y <- ys,
                               let xname = fileName y,
                                   xname `notElem` map fileName xs]

jmenovac :: String -> String -> String
jmenovac x y
  | x == y = x
  | otherwise = x ++ "|" ++ y

oneTree :: (FileName -> a -> DirTree2 a) -> DirTree a -> DirTree2 a
oneTree f (File name file)   = f name file
oneTree f (Dir name content) = Dir2 name (map (oneTree f) content)

dolu :: Dvoj a -> DirTree2 a
dolu (Oboje dt1 dt2) = mergeTrees dt1 dt2
dolu (Levy dt)       = oneTree LeftFile dt
dolu (Pravy dt)      = oneTree RightFile dt

mergeTrees :: DirTree a -> DirTree a -> DirTree2 a
mergeTrees (File name1 file1) (File name2 file2) = BothFile (jmenovac name1 name2) file1 file2
mergeTrees q1@(Dir name1 content1) q2@(Dir name2 content2) =
    let sez = mergeDirLists content1 content2
        sez2 = map (dolu . snd) sez
    in Dir2 (jmenovac name1 name2) (sortBy (compare `on` fileName) sez2)
