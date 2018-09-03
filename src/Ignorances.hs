module Ignorances (

  makeFilterFce,
  IgnoranceDef
) where

import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Debug.Trace
import           DirScan
import           Text.Regex.Posix
import           TurboWare


type IgnoranceDef = [String]

makeFilterFce :: [String] -> (RevPath -> Bool)
makeFilterFce reglist revpath =
    let fces = (neg <$> reglist) ++
                  [const $ Just True]
    in fromJust $ getFirst (foldMap First (fmap ($ revpath) fces))


-- = trace ("AAAAAAAA " ++ show x) (const True)

-- fce :: String -> (RevPath -> Maybe Bool)


neg :: String -> (RevPath -> Maybe Bool)
neg ('!': regexp) = justTrue $ check regexp
neg regexp        = justFalse  $ check regexp

check :: String -> (RevPath -> Bool)
-- check _ _ = True
check regexp revpath = (replaceBacklashesToSlashes . pth) revpath =~ regexp


justTrue :: (a -> Bool) -> (a -> Maybe Bool)
justTrue fce x = if fce x then Just True else Nothing

justFalse :: (a -> Bool) -> (a -> Maybe Bool)
justFalse fce x = if fce x then Just False else Nothing
