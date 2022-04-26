{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
module Algorithm.Text.KMP where

import qualified Data.Automaton      as A
import           Data.List.NonEmpty  as NE
import qualified Data.Vector         as V
import           Data.Vector.Generic as VG
import           Prelude             as P

data Automaton tok = Automaton {
  pat :: V.Vector tok,
  piF :: V.Vector Int
}

deriving instance (Eq tok,Show tok) => Show (Automaton tok)

-- ^compile use O(|tok|) time to compile an KMP automaton
compile :: (Eq tok) => V.Vector tok -> Automaton tok
compile toks = Automaton toks piF where
  piF = V.fromList $ (-1):[findP k $ piF!(k-1) | k<-[1..VG.length toks-1]]

  findP _ (-1) = 0
  findP k j = if toks!j == toks!k
    then j+1
    else findP k $ piF!j

instance (Eq tok) => A.Automaton (Automaton tok) where
  type instance State (Automaton tok) = Int
  type instance Token (Automaton tok) = tok

  isAccept a = (== (VG.length.pat) a)
  initialState _ = 0
  step a@(Automaton pat piF) c = step' where
    accept = A.isAccept a
    step' s
      | accept s = if s == 0 then 0 else step' (piF!(s-1))
      | pat!s == c = s+1
      | s == 0     = 0
      | otherwise  = step' (piF!s)
