module Dump  (
  dirTreeToStringList

) where

import           System.Directory.Tree


dirTreeToStringList :: Show a =>  (a -> Maybe String) -> DirTree a -> [String]
dirTreeToStringList f (File name a) = [name ++ maybe "" (": " ++) (f a) ]
dirTreeToStringList f (Dir name contents) = ("/ " ++ name) : map ("   "++) (concat (dirTreeToStringList f <$> contents))
dirTreeToStringList f (Failed name exc) = [name ++ "EXC " ++ show exc]
