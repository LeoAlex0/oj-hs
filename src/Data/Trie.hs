module Data.Trie where

import qualified Data.HashMap.Lazy as HM
import qualified Data.Hashable     as H
import qualified Data.Maybe        as M
import           Prelude           as P hiding (elem)

-- | a trie tree, like an compressed strings by its prefix.
data Trie tok
  = Trie
      { ends     :: Bool
      , subTries :: HM.HashMap tok (Trie tok)
      }
  deriving (Show)

-- | /O(1)/, empty is a null TrieTree
empty :: Trie tok
empty = Trie {
  ends=False,
  subTries=HM.empty
}

-- | /O(|toks|)/, insert a string into Trie tree
insert :: (H.Hashable tok,Eq tok) => []tok -> Trie tok -> Trie tok
insert [] tree     = tree {ends=True}
insert (t:ts) tree@Trie{subTries=s} = tree {
  subTries=HM.alter (Just. insert ts.M.fromMaybe empty) t s
}

-- | /O(|toks|)/, finds if 'toks' exist in trie tree
-- >>> "123" `elem` insert "123" empty
elem :: (H.Hashable tok,Eq tok) => []tok -> Trie tok -> Bool
elem [] tr = ends tr
elem (t:ts) Trie{subTries=s} = case HM.lookup t s of
    Nothing      -> False
    Just subTree -> ts `elem` subTree
