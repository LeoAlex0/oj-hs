{-# LANGUAGE TypeFamilies #-}
module Algorithm.Text.KMP where

import qualified Data.Automaton      as A
import           Data.List.NonEmpty  as NE
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
compile pat = Automaton next where
  piF = prefix pat
  n = VG.length pat
  next
    | n == 0    = V.singleton M.empty
    | otherwise = V.fromList $ step <$> [0..n] -- non-empty
  step s
    | s == 0    = goNext
    | s == n    = fallback
    | otherwise = goNext `M.union` fallback
    where
      goNext   = M.singleton (pat!s) (s+1)
      fallback = next!(piF!(s-1))

instance (Eq tok,Ord tok) => A.Automaton (Automaton tok) where
  type instance State (Automaton tok) = S
  type instance Token (Automaton tok) = tok

  isAccept (Automaton next) (S s) = s+1 == V.length next
  initialState _ = S 0
  step (Automaton next) c (S s) = S .fromMaybe 0 $ M.lookup c (next!s)
