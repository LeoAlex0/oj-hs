{-# LANGUAGE TypeFamilies #-}
module Algorithm.Text.KMP where

import qualified Data.Automaton      as A
import           Data.List.NonEmpty  as NE
import qualified Data.Vector         as V
import           Data.Vector.Generic as VG
import           Prelude             as P

data Automaton tok
  = Automaton
      { pat :: V.Vector tok
      , piF :: V.Vector Int
      }
  deriving (Show)

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
  findP c 0 = if toks!0 == c then 1 else 0
  findP c j = if toks!j == c
    then j+1
    else findP c $ piF!(j-1)

-- | compile use O(|tok|) time to compile an KMP automaton
compile :: (Eq tok) => V.Vector tok -> Automaton tok
compile toks = Automaton toks (prefix toks)

instance (Eq tok) => A.Automaton (Automaton tok) where
  type instance State (Automaton tok) = Int
  type instance Token (Automaton tok) = tok

  isAccept a = (== (VG.length.pat) a)
  initialState _ = 0
  step a@(Automaton pat piF) c = step' where
    accept = A.isAccept a
    step' s
      | accept s   = if s == 0 then 0 else step' (piF!(s-1))
      | pat!s == c = s+1
      | s == 0     = 0
      | otherwise  = step' (piF!(s-1))
