{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Data.SegTree.Bench where

import           Control.DeepSeq  (NFData)
import           Control.Monad    (replicateM)
import           Data.Monoid
import           Data.SegTree
import           GHC.Generics     (Generic)
import qualified System.Random    as R
import           Test.Tasty.Bench

deriving instance (R.Random a) => R.Random (Sum a)

newtype Plus a
  = Plus a
  deriving (NFData, Show)

instance (Num a) => Semigroup (Plus a) where (Plus a) <> (Plus b) = Plus (a + b)

instance (Num a) => Monoid (Plus a) where mempty = Plus 0

instance (Num a) => Action (Plus a) (Sum a) where action (Plus x) (Sum s) = Sum (x + s)

type TestTree = SegTree (Plus Int) (Sum Int)

testTree :: Int -> IO TestTree
testTree n = fromList <$> replicateM n R.randomIO

trees :: IO (TestTree, TestTree, TestTree)
trees = (,,) <$> testTree (10 ^ 4) <*> testTree (10 ^ 5) <*> testTree (10 ^ 6)

randomQuery :: TestTree -> IO (Sum Int)
randomQuery tree = do
  let len = size tree
  [s1, s2] <- replicateM 2 $ R.randomRIO (0, len - 1)
  pure $ query (min s1 s2) (max s1 s2) tree

randomApply :: TestTree -> IO TestTree
randomApply tree = do
  [s1, s2] <- replicateM 2 $ R.randomRIO (0, size tree - 1)
  (apply . Plus <$> R.randomIO) <*> pure (min s1 s2) <*> pure (max s1 s2) <*> pure tree

-- Our benchmark harness.
test_segTree :: Benchmark
test_segTree = env trees $ \ ~(tree_1e4, tree_1e5, tree_1e6) ->
  bgroup
    "SegTree"
    [ bgroup
        "Build"
        [ bench "1e4" $ nfAppIO testTree (10 ^ 4),
          bench "1e5" $ nfAppIO testTree (10 ^ 5),
          bench "1e6" $ nfAppIO testTree (10 ^ 6)
        ],
      bgroup
        "Query"
        [ bench "1e4" $ nfAppIO randomQuery tree_1e4,
          bench "1e5" $ nfAppIO randomQuery tree_1e5,
          bench "1e6" $ nfAppIO randomQuery tree_1e6
        ],
      bgroup
        "Apply"
        [ bench "1e4" $ whnfAppIO randomApply tree_1e4,
          bench "1e5" $ whnfAppIO randomApply tree_1e5,
          bench "1e6" $ whnfAppIO randomApply tree_1e6
        ]
    ]
