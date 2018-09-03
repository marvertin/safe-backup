module Ignorances (

  makeFilterFce,
  IgnoranceDef
) where

import           DirScan

type IgnoranceDef = [String]

makeFilterFce :: [String] -> (RevPath -> Bool)
makeFilterFce _ = const True
