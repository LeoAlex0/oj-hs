{-# LANGUAGE BangPatterns #-}

module System.IO.Fast.ST
  ( STScanner
  , stScannerFromByteString
  , nextIntST
  , maybeNextIntST
  , nextIntegerST
  , nextWordST
  , nextByteStringST
  , maybeNextByteStringST
  , nextCharST
  , nextIntListST
  ) where

import           Control.Monad.ST       (ST)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BSU
import           Data.STRef             (STRef, newSTRef, readSTRef, writeSTRef)
import           Data.Word              (Word, Word8)

data STScanner s
  = STScanner !BS.ByteString !Int !(STRef s Int)

stScannerFromByteString :: BS.ByteString -> ST s (STScanner s)
stScannerFromByteString bytes =
  STScanner bytes (BS.length bytes) <$> newSTRef 0

nextIntST :: STScanner s -> ST s Int
nextIntST scanner =
  requireParsed "nextIntST" <$> maybeNextIntST scanner

maybeNextIntST :: STScanner s -> ST s (Maybe Int)
maybeNextIntST scanner@(STScanner bytes _ offsetRef) = do
  hasToken <- skipSpacesST scanner
  if not hasToken
    then pure Nothing
    else do
      offset <- readSTRef offsetRef
      let word8 = BSU.unsafeIndex bytes offset
      if word8 == minusSign
        then do
          writeSTRef offsetRef (offset + 1)
          fmap negate <$> parseUnsignedIntST scanner
        else
          if word8 == plusSign
            then writeSTRef offsetRef (offset + 1) >> parseUnsignedIntST scanner
            else parseUnsignedIntST scanner

nextIntegerST :: STScanner s -> ST s Integer
nextIntegerST scanner@(STScanner bytes _ offsetRef) = do
  hasToken <- skipSpacesST scanner
  if not hasToken
    then inputError "nextIntegerST"
    else do
      offset <- readSTRef offsetRef
      let word8 = BSU.unsafeIndex bytes offset
      if word8 == minusSign
        then do
          writeSTRef offsetRef (offset + 1)
          value <- requireParsed "nextIntegerST" <$> parseUnsignedIntegerST scanner
          pure (-value)
        else
          if word8 == plusSign
            then do
              writeSTRef offsetRef (offset + 1)
              requireParsed "nextIntegerST" <$> parseUnsignedIntegerST scanner
            else requireParsed "nextIntegerST" <$> parseUnsignedIntegerST scanner

nextWordST :: STScanner s -> ST s Word
nextWordST scanner = do
  hasToken <- skipSpacesST scanner
  if not hasToken
    then inputError "nextWordST"
    else requireParsed "nextWordST" <$> parseUnsignedWordST scanner

nextByteStringST :: STScanner s -> ST s BS.ByteString
nextByteStringST scanner =
  requireParsed "nextByteStringST" <$> maybeNextByteStringST scanner

maybeNextByteStringST :: STScanner s -> ST s (Maybe BS.ByteString)
maybeNextByteStringST scanner@(STScanner bytes len offsetRef) = do
  hasToken <- skipSpacesST scanner
  if not hasToken
    then pure Nothing
    else do
      start <- readSTRef offsetRef
      let end = scanTokenEnd bytes len start
      writeSTRef offsetRef end
      pure (Just (BS.take (end - start) (BS.drop start bytes)))

nextCharST :: STScanner s -> ST s Char
nextCharST scanner@(STScanner bytes _ offsetRef) = do
  hasToken <- skipSpacesST scanner
  if not hasToken
    then inputError "nextCharST"
    else do
      offset <- readSTRef offsetRef
      writeSTRef offsetRef (offset + 1)
      pure (toEnum (fromIntegral (BSU.unsafeIndex bytes offset)))

nextIntListST :: Int -> STScanner s -> ST s [Int]
nextIntListST count scanner =
  go count []
  where
    go !remaining !values
      | remaining <= 0 = pure (reverse values)
      | otherwise = do
          value <- nextIntST scanner
          go (remaining - 1) (value : values)

parseUnsignedIntST :: STScanner s -> ST s (Maybe Int)
parseUnsignedIntST scanner@(STScanner bytes len offsetRef) = do
  offset <- readSTRef offsetRef
  if offset < len
    then do
      let word8 = BSU.unsafeIndex bytes offset
      if isDigitWord8 word8
        then do
          let (!value, !end) =
                parseIntDigitsAt bytes len (offset + 1) (digitValue word8)
          writeSTRef offsetRef end
          pure (Just value)
        else pure Nothing
    else pure Nothing

parseUnsignedIntegerST :: STScanner s -> ST s (Maybe Integer)
parseUnsignedIntegerST scanner@(STScanner bytes len offsetRef) = do
  offset <- readSTRef offsetRef
  if offset < len
    then do
      let word8 = BSU.unsafeIndex bytes offset
      if isDigitWord8 word8
        then do
          let (!value, !end) =
                parseIntegerDigitsAt bytes len (offset + 1) (fromIntegral (digitValue word8))
          writeSTRef offsetRef end
          pure (Just value)
        else pure Nothing
    else pure Nothing

parseUnsignedWordST :: STScanner s -> ST s (Maybe Word)
parseUnsignedWordST scanner@(STScanner bytes len offsetRef) = do
  offset <- readSTRef offsetRef
  if offset < len
    then do
      let word8 = BSU.unsafeIndex bytes offset
      if isDigitWord8 word8
        then do
          let (!value, !end) =
                parseWordDigitsAt bytes len (offset + 1) (fromIntegral (digitValue word8))
          writeSTRef offsetRef end
          pure (Just value)
        else pure Nothing
    else pure Nothing

parseIntDigitsAt :: BS.ByteString -> Int -> Int -> Int -> (Int, Int)
parseIntDigitsAt =
  parseDigitsAt (\acc word8 -> acc * 10 + digitValue word8)

parseIntegerDigitsAt :: BS.ByteString -> Int -> Int -> Integer -> (Integer, Int)
parseIntegerDigitsAt =
  parseDigitsAt (\acc word8 -> acc * 10 + fromIntegral (digitValue word8))

parseWordDigitsAt :: BS.ByteString -> Int -> Int -> Word -> (Word, Int)
parseWordDigitsAt =
  parseDigitsAt (\acc word8 -> acc * 10 + fromIntegral (digitValue word8))

parseDigitsAt :: (a -> Word8 -> a) -> BS.ByteString -> Int -> Int -> a -> (a, Int)
parseDigitsAt appendDigit bytes len =
  go
  where
    go !offset !acc
      | offset < len =
          let word8 = BSU.unsafeIndex bytes offset
           in if isDigitWord8 word8
                then go (offset + 1) (appendDigit acc word8)
                else (acc, offset)
      | otherwise = (acc, offset)

skipSpacesST :: STScanner s -> ST s Bool
skipSpacesST (STScanner bytes len offsetRef) = do
  offset <- readSTRef offsetRef
  let !nextOffset = scanPastSpaces bytes len offset
  writeSTRef offsetRef nextOffset
  pure (nextOffset < len)

scanPastSpaces :: BS.ByteString -> Int -> Int -> Int
scanPastSpaces bytes len =
  go
  where
    go !offset
      | offset < len && isSpaceWord8 (BSU.unsafeIndex bytes offset) = go (offset + 1)
      | otherwise = offset

scanTokenEnd :: BS.ByteString -> Int -> Int -> Int
scanTokenEnd bytes len =
  go
  where
    go !offset
      | offset < len && not (isSpaceWord8 (BSU.unsafeIndex bytes offset)) = go (offset + 1)
      | otherwise = offset

digitValue :: Word8 -> Int
digitValue word8 =
  fromIntegral (word8 - zeroChar)

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
  error ("System.IO.Fast.ST." ++ name ++ ": input exhausted or malformed")

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
