module Util.Treex (

 Tree(..),
 zipPath,
 union,
 intersection,
) where

import           Data.Functor.Identity
import qualified Data.Map              as M
import           Data.Monoid
import qualified Data.Set              as S

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

buildx :: Ord k => ([k] -> Either [k] a) -> ([k] -> M.Map k (Tree k a) -> a) -> Tree k a
buildx = bu []
  where
   bu ::  Ord k => [k] -> ([k] -> Either [k] a) -> ([k] -> M.Map k (Tree k a) -> a) -> Tree k a
   bu path fd fn = case fd path of
     Left keys -> let ch = M.fromList (fmap (\k -> (k, bu (k:path) fd fn) ) keys)
                   in Tree (fn path ch) ch
     Right a -> Tree a M.empty

build :: Ord k => ([k] -> Either (S.Set k) a) -> ([k] -> M.Map k (Tree k a) -> a) -> Tree k a
build = bu []
 where
  bu ::  Ord k => [k] -> ([k] -> Either (S.Set k) a) -> ([k] -> M.Map k (Tree k a) -> a) -> Tree k a
  bu path fd fn = case fd path of
    Left keys -> let ch = M.fromList (fmap (\k -> (k, bu (k:path) fd fn) ) (S.elems keys))
                  in Tree (fn path ch) ch
    Right a -> Tree a M.empty

build2 :: Ord k => ([k] -> Either (S.Set k) a) -> ([k] -> M.Map k (Tree k a) -> a) -> Tree k a
build2 fd fn = runIdentity $ buildM (return . fd) (\x y -> return (fn x y))


buildM :: (Monad m, Ord k) => ([k] -> m (Either (S.Set k) a)) -> ([k] -> M.Map k (Tree k a) -> m a) -> m (Tree k a)
buildM = bu []
 where
  bu ::   (Monad m, Ord k) => [k] -> ([k] -> m (Either (S.Set k) a)) -> ([k] -> M.Map k (Tree k a) -> m a) -> m (Tree k a)
  bu path fd fn = fd path >>= (\node -> case node of
    Left keys -> let ch = M.fromList <$> (traverse (\k -> sequence (k, bu (k:path) fd fn) ) (S.elems keys))
                  in Tree <$> (fn path =<< ch) <*> ch
    Right a ->  return $ Tree a M.empty)

lookupx :: Ord k => [k] -> Tree k a -> Maybe (Tree k a)
lookupx [] tree             = Just tree
lookupx (k: ks) (Tree _ ch) = lookupx ks =<< M.lookup k ch
