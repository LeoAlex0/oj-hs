{-# LANGUAGE BangPatterns #-}

module System.IO.Fast
  ( Scanner
  , newScanner
  , scannerFromByteString
  , remainingByteString
  , nextInt
  , maybeNextInt
  , nextInteger
  , nextWord
  , nextByteString
  , maybeNextByteString
  , nextChar
  , nextIntList
  ) where

import qualified Data.ByteString as BS
import           Data.Word       (Word, Word8)

newtype Scanner
  = Scanner { remainingByteString :: BS.ByteString }
  deriving (Eq, Show)

newScanner :: IO Scanner
newScanner =
  scannerFromByteString <$> BS.getContents

scannerFromByteString :: BS.ByteString -> Scanner
scannerFromByteString =
  Scanner

nextInt :: Scanner -> (Int, Scanner)
nextInt scanner =
  case maybeNextInt scanner of
    Just result -> result
    Nothing     -> inputError "nextInt"

maybeNextInt :: Scanner -> Maybe (Int, Scanner)
maybeNextInt (Scanner input) =
  let bytes = skipSpaces input
   in case BS.uncons bytes of
        Nothing -> Nothing
        Just (word8, rest)
          | word8 == minusSign -> do
              (value, remaining) <- parseUnsignedInt rest
              pure (-value, Scanner remaining)
          | word8 == plusSign -> do
              (value, remaining) <- parseUnsignedInt rest
              pure (value, Scanner remaining)
          | otherwise -> do
              (value, remaining) <- parseUnsignedInt bytes
              pure (value, Scanner remaining)

nextInteger :: Scanner -> (Integer, Scanner)
nextInteger (Scanner input) =
  let bytes = skipSpaces input
   in case BS.uncons bytes of
        Just (word8, rest)
          | word8 == minusSign ->
              let (value, remaining) = requireParsed "nextInteger" (parseUnsignedInteger rest)
               in (-value, Scanner remaining)
          | word8 == plusSign ->
              let (value, remaining) = requireParsed "nextInteger" (parseUnsignedInteger rest)
               in (value, Scanner remaining)
        _ ->
          let (value, remaining) = requireParsed "nextInteger" (parseUnsignedInteger bytes)
           in (value, Scanner remaining)

nextWord :: Scanner -> (Word, Scanner)
nextWord (Scanner input) =
  let (value, remaining) =
        requireParsed "nextWord" (parseUnsignedWord (skipSpaces input))
   in (value, Scanner remaining)

nextByteString :: Scanner -> (BS.ByteString, Scanner)
nextByteString scanner =
  case maybeNextByteString scanner of
    Just result -> result
    Nothing     -> inputError "nextByteString"

maybeNextByteString :: Scanner -> Maybe (BS.ByteString, Scanner)
maybeNextByteString (Scanner input) =
  let bytes = skipSpaces input
   in if BS.null bytes
        then Nothing
        else
          let (token, remaining) = BS.span (not . isSpaceWord8) bytes
           in Just (token, Scanner remaining)

nextChar :: Scanner -> (Char, Scanner)
nextChar (Scanner input) =
  case BS.uncons (skipSpaces input) of
    Just (word8, remaining) -> (toEnum (fromIntegral word8), Scanner remaining)
    Nothing                 -> inputError "nextChar"

nextIntList :: Int -> Scanner -> ([Int], Scanner)
nextIntList count =
  go count []
  where
    go !remaining !values !scanner
      | remaining <= 0 = (reverse values, scanner)
      | otherwise =
          let (value, nextScanner) = nextInt scanner
           in go (remaining - 1) (value : values) nextScanner

parseUnsignedInt :: BS.ByteString -> Maybe (Int, BS.ByteString)
parseUnsignedInt bytes =
  case BS.uncons bytes of
    Just (word8, remaining)
      | isDigitWord8 word8 -> Just (parseIntDigits (digitValue word8) remaining)
    _ -> Nothing

parseUnsignedInteger :: BS.ByteString -> Maybe (Integer, BS.ByteString)
parseUnsignedInteger bytes =
  case BS.uncons bytes of
    Just (word8, remaining)
      | isDigitWord8 word8 -> Just (parseIntegerDigits (fromIntegral (digitValue word8)) remaining)
    _ -> Nothing

parseUnsignedWord :: BS.ByteString -> Maybe (Word, BS.ByteString)
parseUnsignedWord bytes =
  case BS.uncons bytes of
    Just (word8, remaining)
      | isDigitWord8 word8 -> Just (parseWordDigits (fromIntegral (digitValue word8)) remaining)
    _ -> Nothing

parseIntDigits :: Int -> BS.ByteString -> (Int, BS.ByteString)
parseIntDigits =
  go
  where
    go !acc bytes =
      case BS.uncons bytes of
        Just (word8, remaining)
          | isDigitWord8 word8 -> go (acc * 10 + digitValue word8) remaining
        _ -> (acc, bytes)

parseIntegerDigits :: Integer -> BS.ByteString -> (Integer, BS.ByteString)
parseIntegerDigits =
  go
  where
    go !acc bytes =
      case BS.uncons bytes of
        Just (word8, remaining)
          | isDigitWord8 word8 -> go (acc * 10 + fromIntegral (digitValue word8)) remaining
        _ -> (acc, bytes)

parseWordDigits :: Word -> BS.ByteString -> (Word, BS.ByteString)
parseWordDigits =
  go
  where
    go !acc bytes =
      case BS.uncons bytes of
        Just (word8, remaining)
          | isDigitWord8 word8 -> go (acc * 10 + fromIntegral (digitValue word8)) remaining
        _ -> (acc, bytes)

digitValue :: Word8 -> Int
digitValue word8 =
  fromIntegral (word8 - zeroChar)

skipSpaces :: BS.ByteString -> BS.ByteString
skipSpaces =
  BS.dropWhile isSpaceWord8

isSpaceWord8 :: Word8 -> Bool
isSpaceWord8 word8 =
  word8 <= spaceChar

isDigitWord8 :: Word8 -> Bool
isDigitWord8 word8 =
  zeroChar <= word8 && word8 <= nineChar

requireParsed :: String -> Maybe a -> a
requireParsed _ (Just value) = value
requireParsed name Nothing   = inputError name

inputError :: String -> a
inputError name =
  error ("System.IO.Fast." ++ name ++ ": input exhausted or malformed")

zeroChar :: Word8
zeroChar = 48

nineChar :: Word8
nineChar = 57

spaceChar :: Word8
spaceChar = 32

plusSign :: Word8
plusSign = 43

minusSign :: Word8
minusSign = 45
