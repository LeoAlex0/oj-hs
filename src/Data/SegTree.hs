{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.SegTree(
  Action(..),SegTree,
  size,query,apply,fromList
) where

import           Control.DeepSeq (NFData)
import           Data.List       (unfoldr)
import           GHC.Generics    (Generic)

class Action a v | v -> a where
  action :: a -> v -> v

data SegTree a v
  = Leaf v
  | Branch Int a v !(SegTree a v) !(SegTree a v)
  deriving (Generic, Show)

instance (NFData a,NFData v) => NFData (SegTree a v)

{-# INLINE size #-}
size :: SegTree a v -> Int
size (Leaf _)           = 1
size (Branch l _ _ _ _) = l

{-# INLINE queryAll #-}
queryAll :: SegTree a v -> v
queryAll (Leaf v)           = v
queryAll (Branch _ _ v _ _) = v

query :: (Monoid v, Action a v) => Int -> Int -> SegTree a v -> v
query l r (Leaf v)
  | l <= 0 && 0 < r = v
  | otherwise = mempty
query l r (Branch s a v pr sf)
  | l <= 0 && s <= r = v
  | lInPr && rInPr = action a $ query l r pr
  | lInPr = action a $ query l mid pr <> query 0 (r - mid) sf
  | otherwise = action a $ query (l - mid) (r - mid) sf
  where
    mid = size pr
    lInPr = l < mid
    rInPr = r <= mid

apply :: (Semigroup a, Semigroup v, Action a v) => a -> Int -> Int -> SegTree a v -> SegTree a v
apply a l r se@(Leaf v)
  | l <= 0 && 0 < r = Leaf (action a v)
  | otherwise = se
apply a l r se@(Branch s a1 v pr sf)
  | l <= 0 && s <= r = Branch s (a <> a1) (action a v) pr sf
  | lInPr && rInPr = Branch s a1 (action a1 $ queryAll npr <> queryAll sf) npr sf
  | lInPr = Branch s a1 (action a1 $ queryAll npr <> queryAll nsf) npr nsf
  | otherwise = Branch s a1 (action a1 $ queryAll pr <> queryAll nsf) pr nsf
  where
    mid = size pr
    npr = apply a l r pr
    nsf = apply a (l - mid) (r - mid) sf
    lInPr = l < mid
    rInPr = r <= mid

fromList :: (Monoid a,Semigroup v,Action a v) => [v] -> SegTree a v
fromList []  = error "empty list cannot be a segtree"
fromList xs = root where
    leaves = Leaf <$> xs
    ([root]:_) = dropWhile (not.null.tail) $ iterate (unfoldr buildUp) leaves
    branch l r = Branch (size l+size r) mempty (queryAll l <> queryAll r) l r
    buildUp []         = Nothing
    buildUp [x]        = Just (x,[])
    buildUp [x,y,z]    = Just ((x `branch` y) `branch` z,[])
    buildUp (x:y:rest) = Just (x `branch` y,rest)
