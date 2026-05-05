{-# LANGUAGE DeriveFunctor #-}

module Algorithm.Text.SuffixAutomaton where

import           Data.List   (inits, isSuffixOf)
import qualified Data.Vector

-- >>> endpos "abcdeabcd" "abc"
endpos :: String -> String -> [Int]
endpos s u = res
  where
    -- TODO: reverse inner
    prefixes = [-1 ..] `zip` inits s
    res = fmap fst . filter ((u `isSuffixOf`) . snd) $ prefixes

newtype Index
  = Index Int

data Jump a
  = Accept
  | Hold a
  | Step a
  deriving (Functor, Show)

newtype Automaton a
  = Automaton (a -> Jump (Automaton a))

run :: Automaton a -> [] a -> Maybe (Automaton a)
run s [] = Just s
run (Automaton s) xt@(x : xs) = case s x of
  Accept  -> Nothing
  Hold s' -> run s' xt
  Step s' -> run s' xs
