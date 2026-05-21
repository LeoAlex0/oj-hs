{-# LANGUAGE BangPatterns #-}

module System.IO.Fast.Bench (test_fastIO) where

import           Control.Monad.ST        (runST)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8   as BSC
import qualified Data.ByteString.Lazy    as LBS
import           Data.Char               (isSpace)
import           Data.List               (foldl')
import qualified System.IO.Fast          as Fast
import qualified System.IO.Fast.ST       as FastST
import qualified System.IO.Fast.Stream   as FastStream
import           Test.Tasty.Bench        (Benchmark, bench, bgroup, env, nf, nfAppIO)
import           Text.Printf             (printf)

test_fastIO :: Benchmark
test_fastIO =
  bgroup
    "FastIO"
    [ inputSmallBenchmarks
    , inputLargeBenchmarks
    , outputSmallBenchmarks
    , outputLargeBenchmarks
    ]

smallCount :: Int
smallCount =
  10 ^ (5 :: Int)

largeCount :: Int
largeCount =
  10 ^ (8 :: Int)

chunkCount :: Int
chunkCount =
  smallCount

chunkRepeats :: Int
chunkRepeats =
  largeCount `quot` chunkCount

inputSmallBenchmarks :: Benchmark
inputSmallBenchmarks =
  env (pure (intInput smallCount)) $ \input ->
    bgroup
      "Input/sum 1e5 Int"
      [ bench "read . words" $ nf sumReadWords input
      , bench "ByteString.readInt" $ nf sumByteStringReadInt input
      , bench "System.IO.Fast.Scanner" $ nf sumFastScanner input
      , bench "System.IO.Fast.STScanner" $ nf sumFastSTScanner input
      , bench "System.IO.Fast.StreamScanner" $ nfAppIO sumFastStreamScanner input
      ]

inputLargeBenchmarks :: Benchmark
inputLargeBenchmarks =
  env (pure (intInput chunkCount)) $ \inputChunk ->
    bgroup
      "Input/sum 1e8 Int in 1e5-token chunks"
      [ bench "ByteString.readInt" $ nf (sumByteStringReadIntRepeated chunkRepeats) inputChunk
      , bench "System.IO.Fast.Scanner" $ nf (sumFastScannerRepeated chunkRepeats) inputChunk
      , bench "System.IO.Fast.STScanner" $ nf (sumFastSTScannerRepeated chunkRepeats) inputChunk
      , bench "System.IO.Fast.StreamScanner" $ nfAppIO (sumFastStreamScannerRepeated chunkRepeats) inputChunk
      ]

outputSmallBenchmarks :: Benchmark
outputSmallBenchmarks =
  bgroup
    "Output/render 1e5 Int lines"
    [ bench "show/unlines" $ nf renderShowLines smallCount
    , bench "printf/concat" $ nf renderPrintfLines smallCount
    , bench "ByteString.Builder" $ nf renderBuilderLines smallCount
    ]

outputLargeBenchmarks :: Benchmark
outputLargeBenchmarks =
  bgroup
    "Output/render 1e8 Int lines in 1e5-line chunks"
    [ bench "ByteString.Builder/chunked length" $ nf (renderBuilderLinesLengthRepeated chunkRepeats) chunkCount
    ]

intInput :: Int -> BS.ByteString
intInput n =
  BSC.unwords (map (BSC.pack . show) [1 .. n])

sumReadWords :: BS.ByteString -> Int
sumReadWords =
  foldl' (\acc token -> acc + read token) 0 . words . BSC.unpack

sumByteStringReadInt :: BS.ByteString -> Int
{-# NOINLINE sumByteStringReadInt #-}
sumByteStringReadInt =
  go 0
  where
    go !acc bytes =
      case BSC.readInt (BSC.dropWhile isSpace bytes) of
        Just (value, remaining) -> go (acc + value) remaining
        Nothing                 -> acc

sumFastScanner :: BS.ByteString -> Int
{-# NOINLINE sumFastScanner #-}
sumFastScanner =
  go 0 . Fast.scannerFromByteString
  where
    go !acc scanner =
      case Fast.maybeNextInt scanner of
        Just (value, nextScanner) -> go (acc + value) nextScanner
        Nothing                   -> acc

sumFastSTScanner :: BS.ByteString -> Int
{-# NOINLINE sumFastSTScanner #-}
sumFastSTScanner input =
  runST $ do
    scanner <- FastST.stScannerFromByteString input
    go 0 scanner
  where
    go !acc scanner = do
      next <- FastST.maybeNextIntST scanner
      case next of
        Just value -> go (acc + value) scanner
        Nothing    -> pure acc

sumFastStreamScanner :: BS.ByteString -> IO Int
{-# NOINLINE sumFastStreamScanner #-}
sumFastStreamScanner input = do
  scanner <- FastStream.streamScannerFromChunks (byteStringChunks FastStream.defaultStreamChunkSize input)
  sumFastStreamScannerFrom scanner

sumFastStreamScannerFrom :: FastStream.StreamScanner -> IO Int
sumFastStreamScannerFrom =
  go 0
  where
    go !acc scanner = do
      next <- FastStream.maybeNextIntIO scanner
      case next of
        Just value -> go (acc + value) scanner
        Nothing    -> pure acc

renderShowLines :: Int -> BS.ByteString
renderShowLines n =
  BSC.pack (unlines (map show [1 .. n]))

renderPrintfLines :: Int -> BS.ByteString
renderPrintfLines n =
  BSC.pack (concatMap (printf "%d\n") [1 .. n])

renderBuilderLines :: Int -> BS.ByteString
renderBuilderLines n =
  LBS.toStrict (Builder.toLazyByteString (foldMap intLine [1 .. n]))
  where
    intLine value =
      Builder.intDec value <> Builder.word8 10

sumByteStringReadIntRepeated :: Int -> BS.ByteString -> Int
sumByteStringReadIntRepeated repeats =
  sumRepeated repeats sumByteStringReadInt

sumFastScannerRepeated :: Int -> BS.ByteString -> Int
sumFastScannerRepeated repeats =
  sumRepeated repeats sumFastScanner

sumFastSTScannerRepeated :: Int -> BS.ByteString -> Int
sumFastSTScannerRepeated repeats =
  sumRepeated repeats sumFastSTScanner

sumFastStreamScannerRepeated :: Int -> BS.ByteString -> IO Int
sumFastStreamScannerRepeated repeats input = do
  scanner <- FastStream.streamScannerFromChunks repeatedChunks
  sumFastStreamScannerFrom scanner
  where
    inputChunks =
      byteStringChunks FastStream.defaultStreamChunkSize (input <> BSC.singleton ' ')

    repeatedChunks =
      concat (replicate repeats inputChunks)

sumRepeated :: Int -> (BS.ByteString -> Int) -> BS.ByteString -> Int
sumRepeated repeats parser input =
  go repeats 0
  where
    go !remaining !acc
      | remaining <= 0 = acc
      | otherwise =
          let !chunkSum = parser (sameByteString remaining input)
           in go (remaining - 1) (acc + chunkSum)

renderBuilderLinesLengthRepeated :: Int -> Int -> Int
renderBuilderLinesLengthRepeated repeats count =
  go repeats 0
  where
    go !remaining !acc
      | remaining <= 0 = acc
      | otherwise =
          let !chunkLength = BS.length (renderBuilderLines (sameInt remaining count))
           in go (remaining - 1) (acc + chunkLength)

{-# NOINLINE sameByteString #-}
sameByteString :: Int -> BS.ByteString -> BS.ByteString
sameByteString _ =
  id

{-# NOINLINE sameInt #-}
sameInt :: Int -> Int -> Int
sameInt _ =
  id

byteStringChunks :: Int -> BS.ByteString -> [BS.ByteString]
byteStringChunks chunkSize =
  go
  where
    go bytes
      | BS.null bytes = []
      | otherwise =
          let (chunk, remaining) = BS.splitAt chunkSize bytes
           in chunk : go remaining
