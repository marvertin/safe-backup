{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE RecordWildCards #-}

module TreeComparator (
  compareTrees,
  diffCountAndSizes
) where


import           Data.Maybe
import           Lib
import           TurboWare
import           Types
import           Yaba.Data.Differences
import           Yaba.Data.Lodree

compareTrees :: Lodree -> Lodree -> Maybe DirCompare
compareTrees l r
  | hashLodree l == hashLodree r = Nothing
compareTrees (LDir _ ls) (LDir _ rs) = let
   list :: [(FileName, Maybe (FileName, Lodree), Maybe (FileName, Lodree))]
   list = zipMaybe fst fst ls rs
   compareDirs2 (fileName, l, r) =  fmap ((,) fileName) (compareDirs (snd <$> l) (snd <$> r))
  in Just $ QDir (mapMaybe compareDirs2 list)
  where
     compareDirs :: Maybe Lodree -> Maybe Lodree -> Maybe DirCompare
     compareDirs Nothing Nothing   = error "Imposible has happend! No dir, no file"
     compareDirs (Just l) Nothing  = Just$ QLeft l
     compareDirs Nothing (Just r)  = Just$ QRight r
     compareDirs (Just l) (Just r) = compareTrees l r
compareTrees l r = Just $ QBoth l r

-- | dirrenence coune and sizeSpeed
-- | return left count, left size, right count, right size
diffCountAndSizes :: DirCompare -> ((Int, Integer), (Int, Integer))
diffCountAndSizes dirCompare = let MonoidPlus2x2 result = dcas dirCompare in result
  where
    dcas :: DirCompare -> MonoidPlus2x2 Int Integer Int Integer
    dcas (QLeft lodree) = MonoidPlus2x2 (countsize lodree, (0,0))
    dcas (QRight lodree) = MonoidPlus2x2 ((0,0), countsize lodree)
    dcas (QBoth lodreeLeft lodreeRight) = MonoidPlus2x2 (countsize lodreeLeft, countsize lodreeRight)
    dcas (QDir list) = foldMap (dcas . snd) list

    countsize lodree = let Ree{..} = ree lodree in (rcount, rsize)

--diffCountAndSizes (QR--LEft lodree) = let Ree{..} = ree lodree in ((rcount, rsize), (0,0))
--diffCountAndSizes _ = (17, 5000000, 23, 8000000)
