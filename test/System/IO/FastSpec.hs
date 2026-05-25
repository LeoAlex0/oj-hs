{-# LANGUAGE OverloadedStrings #-}

module System.IO.FastSpec where

import           Control.Exception (bracket)
import           Control.Monad.ST  (runST)
import qualified Data.ByteString   as BS
import           Data.Word         (Word)
import           System.Directory  (getTemporaryDirectory, removeFile)
import           System.IO         (Handle, SeekMode (AbsoluteSeek), hClose,
                                    hFlush, hSeek, hSetBinaryMode, openTempFile)
import qualified System.IO.Fast    as Fast
import qualified System.IO.Fast.ST as FastST
import           Test.Hspec        (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "System.IO.Fast" $ do
  it "parses common Online Judge tokens from a ByteString scanner" $ do
    scanner <- Fast.scannerFromByteString "  -12 34\nword Z"
    a <- Fast.nextInt scanner
    b <- Fast.nextWord scanner
    token <- Fast.nextByteString scanner
    charValue <- Fast.nextChar scanner
    end <- Fast.maybeNextInt scanner

    a `shouldBe` (-12)
    b `shouldBe` 34
    token `shouldBe` "word"
    charValue `shouldBe` 'Z'
    end `shouldBe` Nothing

  it "parses a fixed number of Int values" $ do
    scanner <- Fast.scannerFromByteString "1 2 -3 4 rest"
    values <- Fast.nextIntList 4 scanner
    token <- Fast.nextByteString scanner

    values `shouldBe` [1, 2, -3, 4]
    token `shouldBe` "rest"

  it "keeps the mutable scanner reusable without returning state" $ do
    scanner <- Fast.scannerFromByteString "1  2"
    first <- Fast.nextInt scanner
    second <- Fast.nextInt scanner

    first `shouldBe` 1
    second `shouldBe` 2

  it "streams handle input across internal Fast chunks" $ do
    let chunkSize = 32 * 1024
        longToken = BS.replicate (chunkSize + 3) 120
        input = BS.replicate (chunkSize - 1) 32 <> "123 " <> longToken <> " Z"

    withTempInput input $ \handle -> do
      scanner <- Fast.scannerFromHandle handle
      value <- Fast.nextInt scanner
      token <- Fast.nextByteString scanner
      charValue <- Fast.nextChar scanner
      end <- Fast.maybeNextInt scanner

      value `shouldBe` 123
      token `shouldBe` longToken
      charValue `shouldBe` 'Z'
      end `shouldBe` Nothing

  it "parses common tokens with an ST scanner" $ do
    let result = runST $ do
          scanner <- FastST.stScannerFromByteString "  -12 34\nword Z"
          a <- FastST.nextIntST scanner
          b <- FastST.nextWordST scanner
          token <- FastST.nextByteStringST scanner
          charValue <- FastST.nextCharST scanner
          end <- FastST.maybeNextIntST scanner
          pure (a, b, token, charValue, end)

    result `shouldBe` (-12, 34 :: Word, "word", 'Z', Nothing)

withTempInput :: BS.ByteString -> (Handle -> IO a) -> IO a
withTempInput input action =
  bracket create cleanup $ \(_, handle) -> do
    BS.hPut handle input
    hFlush handle
    hSeek handle AbsoluteSeek 0
    action handle
  where
    create = do
      dir <- getTemporaryDirectory
      result@(_, handle) <- openTempFile dir "fastio-spec.input"
      hSetBinaryMode handle True
      pure result

    cleanup (path, handle) = do
      hClose handle
      removeFile path
