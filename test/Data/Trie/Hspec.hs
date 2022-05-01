{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Data.Trie.Hspec where

import qualified Data.Set                 as S
import           Data.Trie                (elem, empty, insert, toList)
import           Data.Trie.QuickCheck
import           Prelude                  hiding (elem)
import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit (fromHUnitTest)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

insertAll = foldr insert empty

spec :: Spec
spec = describe "Data.Trie" $ do
  prop "inserted val is always exist" $
    \x t -> x `elem` insert @Char x t
  prop "(x==y) === x `elem` insert y empty" $
    \x y -> (x == y) === x `elem` insert @Char y empty
  prop "insert is commutative" $
    \x y -> (insert @Char x. insert y $ empty) === (insert y. insert x $ empty)
  prop "all of inserteds string is in trie" $
    \(strs::[String]) ->  all (`elem` insertAll strs) strs
  prop "(Set.toList. Set.fromList) x === (toList.insertAll) x" $
    \(strs::[String]) -> (toList.insertAll) strs === (S.toList.S.fromList) strs
