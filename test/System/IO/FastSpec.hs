{-# LANGUAGE OverloadedStrings #-}

module System.IO.FastSpec where

import           Control.Monad.ST      (runST)
import           Data.Word             (Word)
import qualified System.IO.Fast        as Fast
import qualified System.IO.Fast.ST     as FastST
import qualified System.IO.Fast.Stream as FastStream
import           Test.Hspec            (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "System.IO.Fast" $ do
  it "parses common Online Judge tokens from a ByteString scanner" $ do
    let scanner0 = Fast.scannerFromByteString "  -12 34\nword Z"
        (a, scanner1) = Fast.nextInt scanner0
        (b, scanner2) = Fast.nextWord scanner1
        (token, scanner3) = Fast.nextByteString scanner2
        (charValue, scanner4) = Fast.nextChar scanner3

    a `shouldBe` (-12)
    b `shouldBe` 34
    token `shouldBe` "word"
    charValue `shouldBe` 'Z'
    Fast.maybeNextInt scanner4 `shouldBe` Nothing

  it "parses a fixed number of Int values" $ do
    let (values, scanner) =
          Fast.nextIntList 4 (Fast.scannerFromByteString "1 2 -3 4 rest")

    values `shouldBe` [1, 2, -3, 4]
    fst (Fast.nextByteString scanner) `shouldBe` "rest"

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

  it "streams tokens across chunk boundaries" $ do
    scanner <- FastStream.streamScannerFromChunks ["  -", "12 3", "4\nwo", "rd Z"]
    a <- FastStream.nextIntIO scanner
    b <- FastStream.nextWordIO scanner
    token <- FastStream.nextByteStringIO scanner
    charValue <- FastStream.nextCharIO scanner
    end <- FastStream.maybeNextIntIO scanner

    a `shouldBe` (-12)
    b `shouldBe` (34 :: Word)
    token `shouldBe` "word"
    charValue `shouldBe` 'Z'
    end `shouldBe` Nothing

  it "streams a fixed number of Int values" $ do
    scanner <- FastStream.streamScannerFromChunks ["1 ", "2 -", "3 4 rest"]
    values <- FastStream.nextIntListIO 4 scanner
    token <- FastStream.nextByteStringIO scanner

    values `shouldBe` [1, 2, -3, 4]
    token `shouldBe` "rest"
