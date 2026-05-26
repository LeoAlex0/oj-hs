{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeFamilies          #-}

module Algorithm.Text.AhoCorasick
  ( -- * Compilation
    compile
  , compileTrie
    -- * Automaton
  , Automaton
  ) where

import           Control.DeepSeq     (NFData)
import qualified Data.Array          as Arr
import qualified Data.Automaton      as Auto
import           Data.Coerce         (coerce)
import qualified Data.IntMap.Strict  as IM
import qualified Data.IntSet         as IS
import qualified Data.List           as L
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import qualified Data.Trie           as Trie
import           Data.Trie           (Trie (..))
import           GHC.Generics        (Generic)

----------------------------------------------------------------------
-- Trie compilation
----------------------------------------------------------------------

data TrieTable tok = TrieTable
  { ttSize     :: !Int
  , ttGotos    :: !(IM.IntMap (M.Map tok Int))
  , ttAccept   :: !IS.IntSet
  , ttAncestry :: !(IM.IntMap (Int, tok))
  }

numberTrie :: (Ord tok) => Trie tok -> TrieTable tok
numberTrie = finish . go 0 1 emptyTable
  where
    emptyTable = TrieTable 1 IM.empty IS.empty IM.empty

    finish table@TrieTable{ttSize = n, ttGotos = gs} =
      table { ttGotos = foldr (\i acc -> IM.insertWith (<>) i M.empty acc) gs [0 .. n - 1] }

    go s next table (Trie end kids) =
      let table1 = if end then table { ttAccept = IS.insert s (ttAccept table) } else table
       in snd $ M.foldlWithKey' (visit s) (next, table1) kids

    visit parent (next, table) c child =
      let childId = next
          table1  = table
            { ttSize = next + 1
            , ttGotos = IM.insertWith (<>) parent (M.singleton c childId) (ttGotos table)
            , ttAncestry = IM.insert childId (parent, c) (ttAncestry table)
            }
          table2 = go childId (childId + 1) table1 child
       in (ttSize table2, table2)

buildTable :: (Ord tok) => TrieTable tok -> Auto.FailureTable tok
buildTable TrieTable{ttSize = n, ttGotos = gs, ttAncestry = par} =
  Auto.complete n gotos ancestry
  where
    gotos    = [IM.findWithDefault M.empty i gs | i <- [0 .. n - 1]]
    ancestry = [(p, c) | i <- [1 .. n - 1], let (p, c) = par IM.! i]

acceptArray :: TrieTable tok -> Arr.Array Int Bool
acceptArray TrieTable{ttSize = n, ttAccept = accepts} =
  Arr.listArray (0, n - 1) [i `IS.member` accepts | i <- [0 .. n - 1]]

acceptFromFallbacks :: Arr.Array Int Bool -> Auto.FailureTable tok -> Arr.Array Int Bool
acceptFromFallbacks direct table = inherited
  where
    fallback = Auto.fallbacks table
    (root, hi) = Arr.bounds direct
    inherited = Arr.listArray (root, hi) [acceptsAt i | i <- [root .. hi]]
    acceptsAt i
      | i == root = direct Arr.! i
      | otherwise = direct Arr.! i || inherited Arr.! (fallback Arr.! i)

----------------------------------------------------------------------
-- Automaton
----------------------------------------------------------------------

-- | Aho-Corasick automaton.
data Automaton tok
  = Automaton
      { next   :: Arr.Array S (M.Map tok S)
      , accept :: Arr.Array S Bool
      }
  deriving (Generic, Show)

instance (NFData tok) => NFData (Automaton tok)

-- | state of Aho-Corasick automaton
newtype S
  = S { unS :: Int }
  deriving (Arr.Ix, Eq, Generic, Ord, Show)

instance NFData S

-- | /O(N * log |Σ|)/ where @N@ is total pattern length.
compile :: (Ord tok) => [[tok]] -> Automaton tok
compile = compileTrie . L.foldl' (flip Trie.insert) Trie.empty

compileTrie :: (Ord tok) => Trie tok -> Automaton tok
compileTrie trie = Automaton
  { next   = Arr.ixmap (S 0, S (n - 1)) unS $ coerce rawNext
  , accept = Arr.ixmap (S 0, S (n - 1)) unS rawAccept
  }
  where
    trieTable = numberTrie trie
    n         = ttSize trieTable
    failTable = buildTable trieTable
    rawNext   = Auto.transitions failTable
    rawAccept = acceptFromFallbacks (acceptArray trieTable) failTable

instance (Ord tok) => Auto.Automaton (Automaton tok) where
  type State (Automaton tok) = S
  type Token (Automaton tok) = tok

  isAccept auto s = accept auto Arr.! s
  initialState _  = S 0
  step auto c s   = Auto.initialState auto `fromMaybe` (next auto Arr.! s M.!? c)
