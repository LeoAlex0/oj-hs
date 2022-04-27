{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Data.EulerTourTree where

import qualified Control.Applicative.Combinators as CAC
import           Control.Monad
import qualified Control.Monad.State.Lazy        as MS
import qualified Data.FingerTree                 as FT
import qualified Data.Foldable                   as F
import qualified Data.List                       as L
import qualified Data.Maybe                      as MB
import qualified Data.Monoid                     as M
import qualified Data.Set                        as S
import qualified Data.Tree                       as T

searchM :: MonadPlus m => FT.Measured v a => (v -> v -> Bool) ->FT.FingerTree v a -> m (FT.FingerTree v a,a,FT.FingerTree v a)
searchM f tree = case FT.search f tree of
  FT.Position pr a sf -> pure (pr,a,sf)
  _                   -> mzero

initSafe :: FT.Measured v a => FT.FingerTree v a -> FT.FingerTree v a
initSafe tree = case FT.viewr tree of
  init FT.:> _ -> init
  _            -> tree

tailSafe :: FT.Measured v a => FT.FingerTree v a -> FT.FingerTree v a
tailSafe tree = case FT.viewl tree of
  _ FT.:< tail -> tail
  _            -> tree

newtype EulerTourNode node
  = EulerTourNode node
deriving instance (Eq node) => Eq (EulerTourNode node)
deriving instance (Ord node) => Ord (EulerTourNode node)
deriving instance (Show node) => Show (EulerTourNode node)

data EulerTourMonoid node
  = EulerTourMonoid (M.First node) (S.Set (node, node)) (M.Last node) (S.Set node) (M.Sum Int)
deriving instance Show node => Show (EulerTourMonoid node)

instance Ord node => Semigroup (EulerTourMonoid node) where
  EulerTourMonoid a b c d e <> EulerTourMonoid a' b' c' d' e' = result where
    result = EulerTourMonoid (a <> a') (b <> bMid <> b') (c <> c') (d <> d') (e <> e')
    bMid = MB.fromMaybe mempty $ do
      l <- M.getLast c
      f <- M.getFirst a'
      pure $ S.singleton (min l f,max l f)

instance Ord node => Monoid (EulerTourMonoid node) where
  mempty = EulerTourMonoid mempty mempty mempty mempty mempty

instance Ord node => FT.Measured (EulerTourMonoid node) (EulerTourNode node) where
  measure (EulerTourNode node) = EulerTourMonoid (pure node) mempty (pure node) (S.singleton node) (pure 1)

firstVertex :: (MonadPlus m, Ord node) => EulerTourMonoid node -> m node
firstVertex (EulerTourMonoid first _ _ _ _) = maybe mzero pure $ M.getFirst first

allNodes :: Ord node => EulerTourMonoid node -> S.Set node
allNodes (EulerTourMonoid _ _ _ nodes _) = nodes

vertexMember :: Ord node => node -> EulerTourMonoid node -> Bool
vertexMember node (EulerTourMonoid _ _ _ nodes _) = S.member node nodes

edgeMember :: Ord node => (node,node) -> EulerTourMonoid node -> Bool
edgeMember (u,v) (EulerTourMonoid _ edges _ _ _) = S.member (min u v,max u v) edges

tourSize :: EulerTourMonoid node -> Int
tourSize (EulerTourMonoid _ _ _ nodes _) = S.size nodes

-- | Euler-tour implementation of a tree structure. It is parameterized by a node type @node@.
--
-- Requirements:
--
-- - @node@ is ordered
-- - node values are unique
data EulerTourTree node where
  EulerTourTree :: Ord node => FT.FingerTree (EulerTourMonoid node) (EulerTourNode node) -> EulerTourTree node

instance Ord node => FT.Measured (S.Set (node,node),S.Set node,M.Sum Int) (EulerTourTree node) where
  measure (EulerTourTree tree) = let EulerTourMonoid _ edges _ nodes size = FT.measure tree in (edges,nodes,size)

instance Foldable EulerTourTree where
  foldMap f etTree@(EulerTourTree _) = MB.maybe mempty (foldMap f) $ toTree etTree

deriving instance Eq node => Eq (EulerTourTree node)
deriving instance Ord node => Ord (EulerTourTree node)
deriving instance Show node => Show (EulerTourTree node)

empty :: Ord node => EulerTourTree node
empty = EulerTourTree FT.empty

singleton :: Ord node => node -> EulerTourTree node
singleton = EulerTourTree .FT.singleton .EulerTourNode

-- >>> let tree = T.Node 1 [T.Node 2 [T.Node 4 []],T.Node 3 []]
-- >>> FT.measure $ MB.fromJust $ fromTree tree
-- (fromList [(1,2),(1,3),(2,4)],fromList [1,2,3,4],Sum {getSum = 7})
fromTree :: (MonadPlus m, Ord node) => T.Tree node -> m (EulerTourTree node)
fromTree tree = do
  guard $ allUnique $ F.toList tree
  pure $ EulerTourTree $ fromTree' tree
  where
    allUnique = all ((==1) . length). L.group. L.sort
    fromTree' (T.Node node forest) = n FT.<| mconcat ((FT.|> n).fromTree' <$> forest) where
      n = EulerTourNode node

type Parser m node = (MonadPlus m, MS.MonadState (FT.FingerTree (EulerTourMonoid node) (EulerTourNode node)) m)

-- | /O(n)/ Deconstruct an Euler tour tree into a 'Data.Tree'.
toTree :: (MonadPlus m, Ord node) => EulerTourTree node -> m (T.Tree node)
toTree (EulerTourTree fingerTree) = MS.evalStateT parser fingerTree where
  parser = do
    EulerTourNode node <- anyToken
    forest <- parser `CAC.endBy` try (token $ EulerTourNode node)
    pure $ T.Node node forest
  anyToken :: Ord node => Parser m node => m (EulerTourNode node)
  anyToken = do
    tree <- MS.get
    case FT.viewl tree of
      node FT.:< tree' -> MS.put tree' >> pure node
      _                -> mzero
  token x = do
    t <- anyToken
    guard (t == x)
    pure t
  try f = do
    a <- MS.get
    f CAC.<|> (MS.put a >> mzero)

root :: (MonadPlus m,Ord node) => EulerTourTree node -> m node
root (EulerTourTree tree) = firstVertex $ FT.measure tree

member :: Ord node => node -> EulerTourTree node -> Bool
member node (EulerTourTree tree) = vertexMember node $ FT.measure tree

size :: Ord node => EulerTourTree node -> Int
size (EulerTourTree fingerTree) = tourSize $ FT.measure fingerTree

-- | /O(log n)/ Return 2 subtrees of @tree@ where @a@ is the subtree of nodes __a__bove @edge@, and @b@ is the subtree of nodes __b__elow @edge@.
--
-- Fail if @edge@ isn't found in @tree@
cutEdge :: (MonadPlus m, Ord node)
        => EulerTourTree node  -- ^ Denoted by @tree@
        -> (node, node)        -- ^ Denoted by @edge@
        -> m (EulerTourTree node, EulerTourTree node)  -- ^ Denoted by @(a, b)@
cutEdge (EulerTourTree tree) e@(a, b) = do
  (left, node, tree') <- searchM p1 tree
  (middle, node', right) <- searchM p2 $ node FT.<| tree'
  let inside = node FT.<| middle
      outside = left <> tailSafe right
  pure (EulerTourTree inside, EulerTourTree outside)
  where p1 before after = edgeMember e before
        p2 before after = not (edgeMember e after)

-- | /O(log n)/ Attach @tree1@ as a child of @node@ in @tree2@.
--
-- Fail if @node@ isn't found in @tree2@, or if @tree1@ and @tree2@ have nodes in common.
splice :: (MonadPlus m, Ord node)
       => EulerTourTree node           -- ^ Denoted by @tree1@
       -> node                         -- ^ Denoted by @node@
       -> EulerTourTree node           -- ^ Denoted by @tree2@
       -> m (EulerTourTree node)
splice (EulerTourTree inviteeTree) node (EulerTourTree hostTree) = do
  guard $ null $ S.intersection (allNodes $ FT.measure inviteeTree) (allNodes $ FT.measure hostTree)
  (left, _, right) <- searchM p hostTree
  let inviteeTree' = if FT.null inviteeTree then inviteeTree else EulerTourNode node FT.<| inviteeTree
  pure $ EulerTourTree $ left <> inviteeTree' <> (EulerTourNode node FT.<| right)
  where p before after = vertexMember node before && not (vertexMember node after)


-- | /O(log n)/ Rotate @tree@ such that @node@ is the new root.
--
-- Fail if @node@ isn't found in @tree@.
reroot :: (MonadPlus m, Ord node)
       => node                 -- ^ Denoted by @node@
       -> EulerTourTree node   -- ^ Denoted by @tree@
       -> m (EulerTourTree node)
reroot node (EulerTourTree tree) = do
  (left, _, tree') <- searchM p1 tree
  (middle, _, right) <- searchM p2 (etNode FT.<| tree')
  pure $ EulerTourTree $ middle <> (etNode FT.<| initSafe right) <> (left FT.|> etNode)
  where p1 before after = vertexMember node before
        p2 before after = not (vertexMember node after)
        etNode = EulerTourNode node
