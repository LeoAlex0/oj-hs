{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module Algorithm.Text.KMP (prefix, compile, Automaton) where

import           Control.DeepSeq     (NFData)
import qualified Data.Array          as A
import qualified Data.Automaton      as A
import           Data.List           as L
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector         as V
import           Data.Vector.Generic as VG
import           GHC.Generics        (Generic)
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
prefix toks = piF
  where
    piF = V.fromList $ 0 : [findP (toks ! k) $ piF ! (k - 1) | k <- [1 .. VG.length toks - 1]]
    findP c j
      | toks ! j == c = j + 1
      | j == 0 = 0
      | otherwise = findP c $ piF ! (j - 1)

newtype Automaton tok
  = Automaton { next :: A.Array S (M.Map tok S) }
  deriving (Generic, Show)

instance (NFData tok) => NFData (Automaton tok)

-- | state of KMP automaton
newtype S
  = S { unS :: Int }
  deriving (A.Ix, Eq, Generic, Ord, Show)

instance NFData S

-- | compile use O(|tok|) time to compile an KMP automaton
compile :: (Eq tok, Ord tok) => [tok] -> Automaton tok
compile [] = Automaton $ A.listArray (S 0, S 0) [M.empty]
compile pat = Automaton next
  where
    n = L.length pat
    piV = prefix (V.fromList pat)
    piF = V.toList piV  -- [π[0], π[1], ..., π[n-1]]

    buildState i
      | i == 0    = M.singleton (pat !! 0) (S 1)
      | i == n    = next A.! S (piF !! (n - 1))
      | otherwise = M.insert (pat !! i) (S (i + 1)) (next A.! S (piF !! (i - 1)))

    next = A.listArray (S 0, S n) [buildState i | i <- [0 .. n]]

instance (Eq tok, Ord tok) => A.Automaton (Automaton tok) where
  type State (Automaton tok) = S
  type Token (Automaton tok) = tok

  isAccept Automaton {next = n} = (== snd (A.bounds n))
  initialState _ = S 0
  step a@Automaton {next = n} c s = A.initialState a `fromMaybe` (n A.! s M.!? c)
