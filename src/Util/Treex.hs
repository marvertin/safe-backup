module Util.Treex (

 Tree(..),
 zipPath,
 union,
 intersection,
) where

import qualified Data.Map    as M
import           Data.Monoid
import qualified Data.Set    as S

data Tree k v = Tree v (M.Map k (Tree k v))

instance Functor (Tree k) where
  fmap fce (Tree x ch) = Tree (fce x) (M.map (fmap fce) ch)

zipPath :: Tree k v -> Tree k ([k], v)
zipPath = zp []
  where
    zp :: [k] -> Tree k v -> Tree k ([k], v)
    zp revpath (Tree x ch) =
         Tree (revpath, x) (M.mapWithKey (\k tree -> zp (k:revpath) tree) ch)


instance Foldable (Tree k) where
  foldMap fce (Tree x ch) = fce x <> foldMap (foldMap fce) ch

instance Traversable (Tree k) where
  traverse fce (Tree x ch) = Tree <$> fce x <*> traverse (traverse fce) ch


union :: Ord k => Tree k a -> Tree k b -> Tree k (Maybe a, Maybe b)
union treeA treeB = uni (fmap (\a -> (Just a, Nothing)) treeA)
                        (fmap (\b -> (Nothing, Just b)) treeB)
  where
    uni :: Ord k => Tree k (Maybe a, Maybe b) -> Tree k (Maybe a, Maybe b) -> Tree k (Maybe a, Maybe b)
    uni (Tree (x, _) xch) (Tree (_, y) ych) = Tree (x, y) (M.unionWith uni xch ych)


intersection :: Ord k => Tree k a -> Tree k b -> Tree k (a, b)
intersection (Tree x xch) (Tree y ych) = Tree (x, y) (M.intersectionWith intersection xch ych)

{-
build :: ([k] -> Either [k] a) -> ([k] -> M.Map k (Tree k a) -> a) -> Tree k a
build = bu []
  where
   bu :: [k] -> ([k] -> Either [k] a) -> ([k] -> M.Map k (Tree k a) -> a) -> Tree k a
   bu path fd fn = case fd path of
     Left keys -> fn path (M.fromList (fmap (\k -> (k, bu (k:path) fd fn) ) keys))
     Right a -> Tree a M.empty
-}
{-
lookupx :: [k] -> Tree k a -> Maybe (Tree k a)
lookupx [] tree      = Just tree
lookupx (k: ks) (Tree _ ch) = case M.lookup ch of
  Nothing      -> Nothing
  (Just tree2) -> lookupx tree2
-}
