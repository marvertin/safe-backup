{-# LANGUAGE FlexibleInstances #-}

module Dump  (
  dirTreeToStringList

) where

import qualified Data.Map              as M
import           System.Directory.Tree
import           TurboWare


dirTreeToStringList :: Show a =>  (a -> Maybe String) -> DirTree a -> [String]
dirTreeToStringList f (File name a) = [name ++ maybe "" (": " ++) (f a) ]
dirTreeToStringList f (Dir name contents) = ("/ " ++ name) : map ("   "++) (concat (dirTreeToStringList f <$> contents))
dirTreeToStringList f (Failed name exc) = [name ++ "EXC " ++ show exc]


--instance (Show k, Show v) => Dumpable (M.Map k v) where
--  toDump m = map (\(k,v) -> show k ++ " = " ++ show v) (M.toList m)
