{-# LANGUAGE TypeFamilies #-}
module Algorithm.Text.KMP(prefix,compile,Automaton) where

import qualified Data.Automaton      as A
import           Data.List           as L
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector         as V
import           Data.Vector.Generic as VG
import           Prelude             as P

-- | prefix function of a string, which means:
--
-- \[
-- \pi(i) = \begin{cases}
--    0 & i = 0 \\
--    \displaystyle\max_{k=1}^i\{k|s[0..k-1] = s[i-(k-1)..i]\} & otherwise
-- \end{cases}
-- \]
prefix :: (Eq tok) => V.Vector tok -> V.Vector Int
prefix toks = piF where
  piF = V.fromList $ 0:[findP (toks!k) $ piF!(k-1) | k<-[1..VG.length toks-1]]
  findP c j
    | toks!j == c = j+1
    | j==0        = 0
    | otherwise   = findP c $ piF!(j-1)

newtype Automaton tok
  = Automaton { next :: V.Vector (M.Map tok Int) }
  deriving (Show)

-- | state of KMP automaton
newtype S
  = S Int

-- | compile use O(|tok|) time to compile an KMP automaton
compile :: (Eq tok,Ord tok) => V.Vector tok -> Automaton tok
compile pat
  | VG.null pat = Automaton (V.singleton M.empty) -- for null-pattern
  | otherwise   = Automaton next
  where
    n      = VG.length pat
    next   = V.fromList $ snd <$> L.scanl' step (0,goNext 0) [1..n] -- non-empty here
    go s c = fromMaybe 0 $ next!s M.!? c

    -- | state transfer table (goto next, pat!s must exist)
    goNext s = M.singleton (pat!s) (s+1)
    step (patState,_) s
      | s == n    = (patState',fallback)
      | otherwise = (patState',goNext s `M.union` fallback)
      where
        fallback   = next!patState       -- state transfer table (fallback to other state)
        patState'  = go patState (pat!s)

instance (Eq tok,Ord tok) => A.Automaton (Automaton tok) where
  type instance State (Automaton tok) = S
  type instance Token (Automaton tok) = tok

  isAccept (Automaton next) (S s) = s+1 == V.length next
  initialState _ = S 0
  step (Automaton next) c (S s) = S .fromMaybe 0 $ M.lookup c (next!s)
