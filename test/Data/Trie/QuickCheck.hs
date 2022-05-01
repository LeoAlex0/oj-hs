module Data.Trie.QuickCheck where

import           Data.Trie
import           Test.QuickCheck

instance (Eq tok,Ord tok,Arbitrary tok) => Arbitrary (Trie tok) where
  arbitrary = Trie <$> arbitrary <*> arbitrary
