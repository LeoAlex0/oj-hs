{-# LANGUAGE TypeFamilies #-}
module Data.Automaton where

class Automaton (a :: *) where
    type family State a
    type family Token a
    isAccept :: a -> State a -> Bool
    initialState :: a-> State a
    step :: a -> Token a -> State a -> State a

run' :: (Automaton a) => a -> [Token a] -> State a -> State a
run' _ []     = id
run' a (t:ts) = run' a ts.step a t

run :: (Automaton a) => a -> [Token a] -> State a
run a ts = run' a ts (initialState a)
