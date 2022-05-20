{-# LANGUAGE DeriveGeneric #-}
module Data.Trie(Trie,empty,insert,elem,toList) where

import           Control.DeepSeq (NFData)
import qualified Data.Map        as M
import qualified Data.Maybe      as M
import           GHC.Generics    (Generic)
import           Prelude         as P hiding (elem)

-- | a trie tree, like an compressed strings by its prefix.
data Trie tok
  = Trie
      { ends     :: Bool
      , subTries :: M.Map tok (Trie tok)
      }
  deriving (Eq, Generic, Show)
instance (NFData tok) => NFData (Trie tok)

-- | /O(1)/, empty is a null TrieTree
empty :: Trie tok
empty = Trie {
  ends=False,
  subTries=M.empty
}

-- | /O(|toks| * log(t))/, insert a string into Trie tree
insert :: (Eq tok,Ord tok) => []tok -> Trie tok -> Trie tok
insert [] tree     = tree {ends=True}
insert (t:ts) tree@Trie{subTries=s} = tree {
  subTries=M.alter (Just. insert ts.M.fromMaybe empty) t s
}

-- | /O(|toks| * log(t))/, finds if 'toks' exist in trie tree
--
-- >>> "123" `elem` insert "123" empty
-- True
elem :: (Eq tok,Ord tok) => []tok -> Trie tok -> Bool
elem [] tr = ends tr
elem (t:ts) Trie{subTries=s} = case M.lookup t s of
  Nothing      -> False
  Just subTree -> ts `elem` subTree

-- | toList extract all token from Trie, in lexicographical order
--
-- >>> toList.insert "123".insert "124" $ empty
-- ["123","124"]
toList :: (Eq tok,Ord tok) => Trie tok -> [[tok]]
toList Trie{ends=e,subTries=s}
  | e = []:deeper
  | otherwise = deeper
  where deeper = M.foldlWithKey' (\l k v -> l <> ((k:) <$> toList v)) [] s
