module Main where

import qualified Algorithm.KMP.Hspec
import qualified Data.Trie.Hspec
import           Test.Hspec

main :: IO ()
main = hspec $ do
    Algorithm.KMP.Hspec.spec
    Data.Trie.Hspec.spec
