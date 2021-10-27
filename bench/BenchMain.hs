{-# LANGUAGE CPP #-}

module Main where

import Criterion.Main (defaultMain)
import Data.FingerTree.Bench

-- Our benchmark harness.
main = defaultMain [benchFingerTree]
