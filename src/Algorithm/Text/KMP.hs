{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Algorithm.Text.KMP (prefix, compile, Automaton) where

import           Control.DeepSeq     (NFData)
import           Control.Monad.ST    (ST)
import qualified Data.Array          as A
import qualified Data.Automaton      as A
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import           GHC.Generics        (Generic)

-- | prefix function of a string, which means:
--
-- \[
-- \pi(i) = \begin{cases}
--    0 & i = 0 \\
--    \displaystyle\max_{k=1}^i\{k|s[0..k-1] = s[i-(k-1)..i]\} & otherwise
-- \end{cases}
-- \]
prefix :: forall tok. (Eq tok) => V.Vector tok -> V.Vector Int
prefix toks
  | n == 0 = V.singleton 0
  | otherwise = V.create $ do
      piM <- MV.new n
      MV.write piM 0 0
      go piM 1
      pure piM
  where
    n = V.length toks

    go :: forall s. MV.MVector s Int -> Int -> ST s ()
    go piM !k
      | k == n = pure ()
      | otherwise = do
          j <- MV.read piM (k - 1)
          p <- findP piM (toks V.! k) j
          MV.write piM k p
          go piM (k + 1)

    findP :: forall s. MV.MVector s Int -> tok -> Int -> ST s Int
    findP piM c !j
      | toks V.! j == c = pure (j + 1)
      | j == 0 = pure 0
      | otherwise = MV.read piM (j - 1) >>= findP piM c

newtype Automaton tok
  = Automaton { next :: A.Array S (M.Map tok S) }
  deriving (Generic, Show)

instance (NFData tok) => NFData (Automaton tok)

-- | state of KMP automaton
newtype S
  = S { unS :: Int }
  deriving (A.Ix, Eq, Generic, Ord, Show)

instance NFData S

-- | compile use O(|tok|) time to compile an KMP automaton.
--
-- Compute the prefix function once, then build the completed transition table.
-- This keeps query-time transitions unchanged while avoiding map lookups during
-- fallback construction.
compile :: (Eq tok, Ord tok) => [tok] -> Automaton tok
compile pat = Automaton next
  where
    toks = V.fromList pat
    piF  = prefix toks
    n    = V.length toks

    next = A.listArray (S 0, S n) [nextAt i | i <- [0 .. n]]

    nextAt 0
      | n == 0 = M.empty
      | otherwise = M.singleton (toks V.! 0) (S 1)
    nextAt i
      | i == n = fallback i
      | otherwise = M.singleton (toks V.! i) (S (i + 1)) <> fallback i

    fallback i = next A.! S (piF V.! (i - 1))

instance (Eq tok, Ord tok) => A.Automaton (Automaton tok) where
  type State (Automaton tok) = S
  type Token (Automaton tok) = tok

  isAccept Automaton {next = n} = (== snd (A.bounds n))
  initialState _ = S 0
  step a@Automaton {next = n} c s = A.initialState a `fromMaybe` (n A.! s M.!? c)
