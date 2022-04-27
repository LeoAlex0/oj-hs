module Solution.Luogu.P1383 where

import           Control.Monad           (foldM_)
import           Data.FingerTree
import           Data.FingerTree.Measure
import           Data.Functor            (($>))
import           Text.Printf             (printf)

type Rope = FingerTree Size (Value Char)

foldM_' z l f = foldM_ f z l

main = do
  n <- readLn :: IO Int
  fromList [Value (fromList [] :: Rope)] `foldM_'` [1 .. n] $ \seq _ -> do
    [[op], chr] <- words <$> getLine
    case op of
      'T' -> let Value latest :< _ = viewl seq in pure $ Value (latest |> Value (head chr)) <| seq
      'U' -> let Position _ at _ = search (\fr _ -> fr > Size (read chr)) seq in pure $ at <| seq
      'Q' ->
        let Value latest :< _ = viewl seq
            Position _ (Value at) _ = search (\fr _ -> fr >= Size (read chr)) latest
         in printf "%c\n" at $> seq
      _ -> error "unknown operation"
