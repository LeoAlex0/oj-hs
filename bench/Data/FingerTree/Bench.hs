{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Data.FingerTree.Bench where

import           Control.DeepSeq         (NFData)
import           Control.Monad           (replicateM)
import           Criterion.Main
import           Data.FingerTree
import           Data.FingerTree.Measure
import           GHC.Generics            (Generic)
import         qualified  System.Random as R

deriving instance R.Random a => R.Random (Value a)

trees :: IO (FingerTree Size (Value Int), FingerTree Size (Value Int), FingerTree Size (Value Int))
trees = do
  raw_1e4 <- replicateM (10 ^ 4) R.randomIO
  raw_1e5 <- replicateM (10 ^ 5) R.randomIO
  raw_1e6 <- replicateM (10 ^ 6) R.randomIO
  pure (fromList raw_1e4,fromList raw_1e5,fromList raw_1e6)

-- Our benchmark harness.
-- >>> [1..1e5] :: FingerTree Size (Value Int)
benchFingerTree = env trees $ \ ~(tree_1e4, tree_1e5, tree_1e6) ->
  bgroup
    "FingerTree"
    [ bgroup
        "Split"
        [ bench "1e4" $ nf (fst . split (> 5 * 10 ^ 3)) tree_1e4,
          bench "1e5" $ nf (fst . split (> 5 * 10 ^ 4)) tree_1e5,
          bench "1e6" $ nf (fst . split (> 5 * 10 ^ 5)) tree_1e6
        ],
      bgroup
        "Concat"
        [ bench "1e4 <> 1e4" $ nf (tree_1e4 <>) tree_1e4,
          bench "1e5 <> 1e5" $ nf (tree_1e5 <>) tree_1e5,
          bench "1e6 <> 1e6" $ nf (tree_1e6 <>) tree_1e6,
          bench "1e4 <> 1e6" $ nf (tree_1e4 <>) tree_1e6,
          bench "1e6 <> 1e4" $ nf (<> tree_1e4) tree_1e6
        ],
      bgroup
        "PushL"
        [ bench "1e4" $ nf (0 <|) tree_1e4,
          bench "1e5" $ nf (0 <|) tree_1e5,
          bench "1e6" $ nf (0 <|) tree_1e6
        ],
      bgroup
        "PushR"
        [ bench "1e4" $ nf (|> 0) tree_1e4,
          bench "1e5" $ nf (|> 0) tree_1e5,
          bench "1e6" $ nf (|> 0) tree_1e6
        ]
    ]
