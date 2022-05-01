{-# LANGUAGE TypeApplications #-}
module Algorithm.KMP.Hspec where
import           Algorithm.Text.KMP        (compile, prefix)
import           Data.Automaton            (Automaton (isAccept), run)
import           Data.List                 (isSuffixOf)
import           Data.Vector               as V (Vector, fromList, length,
                                                 toList, (!))
import           Test.Hspec                (Spec, describe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           (Arbitrary (arbitrary),
                                            Args (maxSize), choose, disjoin,
                                            expectFailure, forAll, getSize,
                                            vector, within, (.&&.), (===),
                                            (==>))
import           Test.QuickCheck.Modifiers (Positive (Positive))


instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary

spec:: Spec
spec = describe "Algorithm.KMP" $ do
  -- Prefix function
  prop "prefix function must meet the define: case [0]" $
    \str -> within (10^3) $ prefix @Char str!0 === 0
  prop "prefix function must meet the define:" $
    \str (Positive i) -> let
      pI = (prefix @Char str!i)
      pred k = [str!j|j<-[0..k-1]]==[str!j|j<-[i-(k-1)..i]]
      in
        within (10^3) $ i < V.length str ==> pred pI .&&. forAll (choose (pI+1,i)) (not.pred)

  -- Automaton
  prop "KMP automaton can accpet any suffix" $
    \str1 str2 -> let auto = compile @Char str1 in
      (isAccept auto.run auto.V.toList) (str2<>str1)
  prop "KMP automaton deny if not a suffix" $
    \str1 str2 -> let auto = compile @Char str1 in
      not (V.toList str1 `isSuffixOf` V.toList str2) ==> (not.isAccept auto.run auto.V.toList) str2
