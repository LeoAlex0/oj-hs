{-# LANGUAGE TypeFamilies #-}
module Data.Automaton where

class Automaton (a :: *) where
    type family State a
    type family Token a
    isAccept :: a -> State a -> Bool
    initialState :: a-> State a
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
