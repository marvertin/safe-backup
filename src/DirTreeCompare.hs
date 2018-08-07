module DirTreeCompare
    (
    DirTree2(AddFord, DeleteFord, Dir2, Nic2),
    mergeTrees,
    leftCount,
    rightCount,
    fileNamex,
    ) where

import           DirTree
import           Lib
import           System.Directory.Tree (DirTree (Dir, Failed, File), FileName)
import           Text.Printf           (printf)

import           Crypto.Hash.SHA1      (hashlazy)
import           Data.Function
import           Data.List             (sortBy)
import           Data.Maybe


data Dvoj a = Levy (DirTree a) | Pravy (DirTree a) | Oboje (DirTree a) (DirTree a) deriving (Show, Eq, Ord)

data DirTree2 a = AddFord (DirTree a)
              | DeleteFord (DirTree a)
              | Dir2 FileName [DirTree2 a]
              | Nic2 FileName a
              deriving (Show, Eq, Ord)

count :: DirTree a -> Int
count File {} = 1
count Failed {} = 1
count (Dir _ content) = length content

leftCount :: DirTree2 a -> Int
leftCount (AddFord x) = count x
leftCount (DeleteFord x) = 0
leftCount Nic2 {}      = 1
leftCount (Dir2 _ content) = sum $ map leftCount content

rightCount :: DirTree2 a -> Int
rightCount (AddFord x) = 0
rightCount (DeleteFord x) = count x
rightCount Nic2 {}      = 1
rightCount (Dir2 _ content) = sum $ map rightCount content

{-
rightCount LeftFile {}      = 0
rightCount BothFile {}      = 1
rightCount RightFile {}     = 1
rightCount (Dir2 _ content) = sum $ map rightCount content
-}
class  HasFileName a  where
  fileNamex :: a -> FileName

instance HasFileName (DirTree2 a) where
  fileNamex (AddFord dt)   = fileNamex dt
  fileNamex (DeleteFord dt)   = fileNamex dt
  fileNamex (Dir2 name _)       = name
  fileNamex (Nic2 name _)       = name

instance HasFileName (DirTree a) where
  fileNamex (Dir name _)    = name
  fileNamex (File name _)   = name
  fileNamex (Failed name _) = name


mergeDirLists :: [DirTree a] -> [DirTree a] -> [(FileName, Dvoj a)]
mergeDirLists xs ys = sortBy (compare `on` fst) [ (xname, Oboje x y) | x <- xs, y <- ys,
                              let xname = fileNamex x, let yname = fileNamex y,
                              xname == yname ] -- it is quadratic !
                  ++  [ (xname, Levy x) | x <- xs,
                              let xname = fileNamex x,
                               xname `notElem` map fileNamex ys]
                  ++  [ (xname, Pravy y) | y <- ys,
                               let xname = fileNamex y,
                                   xname `notElem` map fileNamex xs]

jmenovac :: String -> String -> String
jmenovac x y
  | x == y = x
  | otherwise = x ++ "|" ++ y


dolu :: Eq a => (a -> a -> Bool) -> Dvoj a -> Maybe (DirTree2 a)
dolu (=^=) (Oboje dt1 dt2) = mergeTrees (=^=) dt1 dt2
dolu _ (Levy dt)       = Just (AddFord dt)
dolu _ (Pravy dt)      = Just (DeleteFord dt)


mergeTrees :: Eq a => (a -> a -> Bool) -> DirTree a -> DirTree a -> Maybe (DirTree2 a)
mergeTrees (=^=) q1@(File name1 file1) (File name2 file2)
  | file1 =^= file2 = Nothing -- Just (Nic2 (jmenovac name1 name2) file1)
  | otherwise = Just(AddFord q1)
mergeTrees comp q1@(Dir name1 content1) q2@(Dir name2 content2) =
    let sez = mergeDirLists content1 content2
        sez2 = mapMaybe (dolu comp . snd) sez
    in if null sez2 then Nothing
       else Just (Dir2 (jmenovac name1 name2) (sortBy (compare `on` fileNamex) sez2))
mergeTrees _ q1@(Dir _ _) (File _ _) =  Just (AddFord q1)
mergeTrees _ q1@(File _ _) (Dir _ _) =  Just (AddFord q1)
