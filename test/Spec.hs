module Main where

import qualified Data.Trie.Hspec
import           Test.Hspec

main :: IO ()
main = hspec $ do Data.Trie.Hspec.spec
