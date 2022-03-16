{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Data.SegTree.Bench where

import           Control.DeepSeq (NFData)
import           Control.Monad   (replicateM)
import           Criterion.Main
import           Data.Monoid
import           Data.SegTree
import           GHC.Generics    (Generic)
import           System.Random   (Random (randomIO))

deriving instance Random a => Random (Sum a)

newtype Plus a = Plus a deriving (Show,NFData)
instance Num a => Semigroup (Plus a) where (Plus a) <> (Plus b) = Plus (a+b)
instance Num a => Monoid (Plus a) where mempty = Plus 0
instance Num a => Action (Plus a) (Sum a) where action (Plus x) (Sum s) = Sum (x+s)

type TestTree = SegTree (Plus Int) (Sum Int)

trees :: IO (TestTree,TestTree,TestTree)
trees = do
  raw_1e4 <- replicateM (10 ^ 4) randomIO
  raw_1e5 <- replicateM (10 ^ 5) randomIO
  raw_1e6 <- replicateM (10 ^ 6) randomIO
  pure (fromList raw_1e4,fromList raw_1e5,fromList raw_1e6)

-- Our benchmark harness.
benchST = env trees $ \ ~(tree_1e4, tree_1e5, tree_1e6) ->
  bgroup
    "SegTree"
    [ bgroup
        "Query"
        [ bench "1e4" $ whnf (query (3*10^3) (6*10^3)) tree_1e4,
          bench "1e5" $ whnf (query (3*10^4) (6*10^4)) tree_1e5,
          bench "1e6" $ whnf (query (3*10^5) (6*10^5)) tree_1e6
        ],
      bgroup
        "Apply"
        [ bench "1e4 <> 1e4" $ whnf (apply (Plus 1) (3*10^3) (6*10^3)) tree_1e4,
          bench "1e5 <> 1e5" $ whnf (apply (Plus 1) (3*10^4) (6*10^4)) tree_1e5,
          bench "1e6 <> 1e6" $ whnf (apply (Plus 1) (3*10^5) (6*10^5)) tree_1e6
        ]
    ]
