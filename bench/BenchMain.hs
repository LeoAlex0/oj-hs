module Main where

import qualified BenchDiscover
import           Test.Tasty.Bench (defaultMain)

main :: IO ()
main = BenchDiscover.tests >>= defaultMain . pure
