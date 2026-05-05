module Main where

import qualified Algorithm.KMP.Bench   as KMP
import           Criterion.Main        (defaultMain)
import qualified Data.FingerTree.Bench as FT
import qualified Data.SegTree.Bench    as ST

-- Our benchmark harness.
main = defaultMain [FT.benchFingerTree, ST.benchST, KMP.benchST]
