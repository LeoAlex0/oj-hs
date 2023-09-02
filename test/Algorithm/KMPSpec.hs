{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Algorithm.KMPSpec where

import Algorithm.Text.KMP (compile, prefix)
import Data.Automaton (Automaton (isAccept), run)
import qualified Data.ByteString as BS (isSuffixOf, unpack)
import Data.List as L (isSuffixOf)
import Data.String (IsString (..))
import Data.Vector as V
  ( Vector,
    fromList,
    length,
    null,
    toList,
    (!),
  )
import Test.HUnit ((@?=))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( ASCIIString (ASCIIString),
    Arbitrary (arbitrary),
    Args (maxSize),
    NonNegative (NonNegative),
    PrintableString (PrintableString),
    choose,
    disjoin,
    expectFailure,
    forAll,
    getSize,
    vector,
    within,
    (.&&.),
    (===),
    (==>),
  )
import Test.QuickCheck.Modifiers (Positive (Positive))

instance (Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary

spec :: Spec
spec = describe "Algorithm.KMP" $ do
  describe "prefix function" $ do
    it "simple test case" $ do
      (prefix . V.fromList) "" `shouldBe` [0]
      (prefix . V.fromList) "aabaaab" `shouldBe` [0, 1, 0, 1, 2, 2, 3]
    prop "prefix function must meet the define: case [0]" $
      \str -> within (10 ^ 6) $ prefix @Char str ! 0 === 0
    prop "prefix function must meet the define:" $
      \str -> within (10 ^ 6) $ (not . V.null) str ==> forAll (choose (0, V.length str - 1)) $ \i ->
        let pI = (prefix @Char str ! i)
            pred k = [str ! j | j <- [0 .. k - 1]] == [str ! j | j <- [i - (k - 1) .. i]]
         in pred pI .&&. (pI < i ==> forAll (choose (pI + 1, i)) (not . pred))

  describe "KMP automaton" $ do
    it "simple test case" $ do
      let auto = compile "aba"
          match = isAccept auto . run auto
      match "ab" `shouldBe` False
      match "aba" `shouldBe` True
      match "ababa" `shouldBe` True
      match "ababc" `shouldBe` False
    prop "can accpet any suffix" $
      \(PrintableString str1) (PrintableString str2) ->
        let s1 : [s2] = fromString <$> [str1, str2]
            auto = (compile . BS.unpack) s1
         in (within (10 ^ 6) . isAccept auto . run auto . BS.unpack) (s2 <> s1)
    prop "deny if not a suffix" $
      \(PrintableString str1) (PrintableString str2) ->
        let s1 : [s2] = fromString <$> [str1, str2]
            auto = (compile . BS.unpack) s1
         in within (10 ^ 6) $ not (s1 `BS.isSuffixOf` s2) ==> (not . isAccept auto . run auto . BS.unpack) s2
    prop "can used in binary string" $
      \(str1 :: [Bool]) str2 ->
        let auto = compile str1
         in (within (10 ^ 4) . isAccept auto . run auto) (str2 <> str1)
    prop "and deny if not a binary suffix" $
      \(str1 :: [Bool]) str2 ->
        let auto = compile str1
         in within (10 ^ 4) $ not (str1 `L.isSuffixOf` str2) ==> (not . isAccept auto . run auto) str2
