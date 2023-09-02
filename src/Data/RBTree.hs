{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.RBTree (RBTree, empty, singleton, insert) where

-- use data Nat = S Nat | Z if u like
import GHC.TypeNats (Natural, type (+))

-- Root is black
-- n means black height (start with 0)
data RBTree a where
  RBTree :: (BBranch n a) -> RBTree a

data RBBranch (n :: Natural) a
  = Red (RBBranch n a)
  | Black (BBranch n a)

data RBranch (n :: Natural) a where
  RBranch :: BBranch n a -> a -> BBranch n a -> RBranch (n + 1) a

data BBranch (n :: Natural) a where
  NIL :: BBranch 0 a
  BBranch :: RBBranch n a -> a -> RBBranch n a -> BBranch (n + 1) a

-- all definition down, implement algorithm

empty :: RBTree a
empty = RBTree NIL

singleton :: a -> RBTree a
singleton x = RBTree (BBranch (Black NIL) x (Black NIL))

data Interval a
  = Before a
  | RightThan a
  | In a a
  | None

--   deriving (Show)

instance Semigroup (Interval a) where
  (<>) :: Interval a -> Interval a -> Interval a
  None <> None = None
  RightThan x <> Before y = In x y
  RightThan x <> In _ y = In x y
  In x _ <> Before y = In x y
  RightThan x <> _ = RightThan x
  _ <> Before y = Before y
  In x _ <> In _ y = In x y
  a <> b = error "none-cross interval merge"

searchMinMaxBy' :: forall (n :: Natural) a. (a -> a -> Ordering) -> a -> RBBranch n a -> Interval a
searchMinMaxBy' _ _ (Black NIL) = None
searchMinMaxBy' cmp x (Black (BBranch l a r)) = case cmp x a of
  EQ -> searchMinMaxBy' cmp a l <> RightThan a <> searchMinMaxBy' cmp a r
  GT -> searchMinMaxBy' cmp a l <> Before a
  LT -> searchMinMaxBy' cmp a r

-- >>> putStrLn "123"
searchMinMaxBy :: (a -> a -> Ordering) -> a -> RBTree a -> Interval a
searchMinMaxBy cmp x (RBTree branch) = searchMinMaxBy' cmp x (Black branch)

insert :: (Ord a) => a -> RBTree a -> RBTree a
insert x (RBTree NIL) = singleton x

-- insert x (RBTree (BBranch l a r)) =undefined
