module Ignorances (

  makeFilterFce,
  IgnoranceDef
) where

import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Debug.Trace
import           DirScan
import           System.FilePath
import           Text.Regex.Posix
import           TurboWare


type IgnoranceDef = [String]

type Expr = String
type Expr1 = String
type Expr2 = String

makeFilterFce :: [Expr2] -> (RevPath -> Bool)
makeFilterFce exprlist revpath =
    let path = (replaceBacklashesToSlashes . pth) revpath
        fces = (neg <$> exprlist) ++ [const $ Just True]
    in fromJust $ getFirst (foldMap First (fmap ($  path) fces))


neg :: Expr2 -> (FilePath -> Maybe Bool)
neg ('i': expr1) = justTrue $ check expr1
neg ('e': expr1) = justFalse $ check expr1
neg __           = const Nothing

check :: Expr1 -> (FilePath -> Bool)
check ('=': expr) path = checkEquality expr path
check ('~': expr) path = checkRegexp expr path
check expr _           = error ("Bat pattern in filter: " ++ expr)
-- check _ _ = True

checkEquality :: Expr -> FilePath -> Bool
checkEquality = (==)


checkRegexp :: Expr -> FilePath -> Bool
checkRegexp regexp path = path =~ wrapregexp regexp

wrapregexp :: String -> String
wrapregexp r = "^" ++ r ++ "$"


justTrue :: (a -> Bool) -> (a -> Maybe Bool)
justTrue fce x = if fce x then Just True else Nothing

justFalse :: (a -> Bool) -> (a -> Maybe Bool)
justFalse fce x = if fce x then Just False else Nothing
