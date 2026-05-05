module Algorithm.KMP.Bench (benchST) where

import           Algorithm.Text.KMP (Automaton, compile)
import           Control.Monad      (replicateM)
import           Criterion          (bench, bgroup, env, nfAppIO)
import           Data.Automaton     (run)
import           System.Random      (randomIO)

testAutomaton :: Int -> IO (Automaton Char)
testAutomaton n = compile <$> testString n

testString :: Int -> IO [Char]
testString n = replicateM n randomIO

-- Our benchmark harness.
benchST = env (testAutomaton (10 ^ 4)) $ \auto ->
  bgroup
    "KMP"
    [ bgroup
        "Compile"
        [ bench "1e4" $ nfAppIO testAutomaton (10 ^ 4),
          bench "1e5" $ nfAppIO testAutomaton (10 ^ 5),
          bench "1e6" $ nfAppIO testAutomaton (10 ^ 6)
        ],
      bgroup
        "Running"
        [ bench "1e4" $ nfAppIO ((run auto <$>) . testString) (10 ^ 4),
          bench "1e5" $ nfAppIO ((run auto <$>) . testString) (10 ^ 5),
          bench "1e6" $ nfAppIO ((run auto <$>) . testString) (10 ^ 6)
        ]
    ]
