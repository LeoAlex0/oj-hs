{-# LANGUAGE TypeFamilies #-}

module Data.Automaton
  ( Automaton (..)
  , scanFrom, scan, runFrom, run
    -- * Self-referential construction (KMP / Aho-Corasick)
  , FailureTable
  , transitions, fallbacks
  , complete
  ) where

import qualified Data.Array as A
import           Data.Kind  (Type)
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)

class Automaton (a :: Type) where
  type State a
  type Token a
  isAccept :: a -> State a -> Bool
  initialState :: a -> State a
  step :: a -> Token a -> State a -> State a

-- | scan an automaton from a customized start state
scanFrom :: (Automaton a) => a -> [Token a] -> State a -> [State a]
scanFrom a ts initial = scanl (flip (step a)) initial ts

scan :: (Automaton a) => a -> [Token a] -> [State a]
scan a ts = scanFrom a ts (initialState a)

-- | run an automaton from a customized start state
runFrom :: (Automaton a) => a -> [Token a] -> State a -> State a
runFrom a ts initial = foldl (flip (step a)) initial ts

-- | run an automaton from automaton's initial state
run :: (Automaton a) => a -> [Token a] -> State a
run a ts = runFrom a ts (initialState a)

----------------------------------------------------------------------
-- Self-referential (knot-tying) automaton construction
----------------------------------------------------------------------

-- | Completed transition table plus failure links.
data FailureTable tok = FailureTable
  { transitions :: A.Array Int (M.Map tok Int)
  , fallbacks   :: A.Array Int Int
  }

-- | Build a transition table via lazy knot-tying — the common core of
-- both KMP and Aho-Corasick.
--
-- Given @n@ states (0 = root), direct goto edges, and an ancestry
-- relation @(parent, label)@ for states @1..n-1@ in index order:
--
-- @
--   fail[i]  =  0                                  if parent = 0
--            =  step(auto, label, fail[parent])     otherwise
--
--   next[0]  = gotos[0]
--   next[i]  = gotos[i] <> next[fail[i]]            (i >= 1)
-- @
--
-- Root's children (depth 1) always fail to the root — otherwise
-- @step(c, 0)@ would return @i@, creating a self-loop.
complete ::
     (Ord tok)
  => Int                    -- ^ number of states @n@
  -> [M.Map tok Int]        -- ^ direct goto edges per state, length @n@
  -> [(Int, tok)]           -- ^ @(parent, label)@ for states @1..n-1@, length @n-1@
  -> FailureTable tok
complete n gotos ancestry = FailureTable next fallback
  where
    fallback = A.listArray (0, n - 1) $
      0 : [if p == 0 then 0 else step c (fallback A.! p) | (p, c) <- ancestry]

    next = A.listArray (0, n - 1) $
      head gotos : zipWith (<>) (tail gotos) (map (next A.!) (tail (A.elems fallback)))

    step c s = fromMaybe 0 $ next A.! s M.!? c
