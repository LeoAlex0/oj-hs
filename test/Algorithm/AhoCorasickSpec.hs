module Algorithm.AhoCorasickSpec where

import qualified Algorithm.Text.AhoCorasick as AC
import qualified Data.Automaton             as Auto
import qualified Data.Trie                  as Trie
import           Test.Hspec                 (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Algorithm.AhoCorasick" $ do
  it "matches a single pattern like a suffix automaton" $ do
    let auto = AC.compile (["aba"] :: [String])
        accepts s = Auto.isAccept auto (Auto.run auto s)
    accepts "ab" `shouldBe` False
    accepts "aba" `shouldBe` True
    accepts "xxaba" `shouldBe` True
    accepts "ababc" `shouldBe` False

  it "inherits accepting states through failure links" $ do
    let auto = AC.compile (["bc", "abcd"] :: [String])
        accepts s = Auto.isAccept auto (Auto.run auto s)
    accepts "ab" `shouldBe` False
    accepts "abc" `shouldBe` True
    accepts "xabc" `shouldBe` True

  it "compiles a Trie explicitly" $ do
    let trie = Trie.insert "bc" $ Trie.insert "abcd" Trie.empty
        auto = AC.compileTrie trie
        accepts s = Auto.isAccept auto (Auto.run auto s)
    accepts "abc" `shouldBe` True
