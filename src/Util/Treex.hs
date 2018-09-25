module Util.Treex (

 Tree(..)
) where

import qualified Data.Map    as M
import           Data.Monoid

data Tree k v = Tree v (M.Map k (Tree k v))

instance Functor (Tree k) where
  fmap fce (Tree x ch) = Tree (fce x) (M.map (fmap fce) ch)

zipPath :: Tree k v -> Tree k ([k], v)
zipPath tree = zp [] tree
  where
    zp :: [k] -> Tree k v -> Tree k ([k], v)
    zp revpath (Tree x ch) =
         Tree (revpath, x) (M.mapWithKey (\k tree -> zp (k:revpath) tree) ch)


instance Foldable (Tree k) where
  foldMap fce (Tree x ch) = fce x <> foldMap (foldMap fce) ch

instance Traversable (Tree k) where
  traverse fce (Tree x ch) = Tree <$> fce x <*> traverse (traverse fce) ch


union :: Tree k a -> Tree k b -> Tree k (Maybe a, Maybe b)
union (Tree x xch) (Tree y ych) = Tree (Just x, Just y) M.empty
