{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE LinearTypes  #-}

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

import           Control.Monad.ST         (ST)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Unsafe   as BSU
import           Data.Primitive.PrimArray (MutablePrimArray, newPrimArray,
                                           readPrimArray, writePrimArray)
import           Data.Word                (Word, Word8)
import           GHC.Exts                 (Multiplicity (Many))

data STScanner s
  = STScanner !BS.ByteString !Int !(MutablePrimArray s Int)

offsetIndex :: Int
offsetIndex =
  0

stScannerFromByteString :: BS.ByteString -> ST s (STScanner s)
{-# INLINE stScannerFromByteString #-}
stScannerFromByteString bytes = do
  offsetRef <- newPrimArray 1
  writePrimArray offsetRef offsetIndex 0
  pure (STScanner bytes (BS.length bytes) offsetRef)

nextIntST :: STScanner s %Many -> ST s Int
{-# INLINE nextIntST #-}
nextIntST scanner =
  requireParsed "nextIntST" <$> maybeNextIntST scanner

maybeNextIntST :: STScanner s %Many -> ST s (Maybe Int)
{-# INLINE maybeNextIntST #-}
maybeNextIntST scanner@(STScanner bytes _ offsetRef) = do
  hasToken <- skipSpacesST scanner
  if not hasToken
    then pure Nothing
    else do
      offset <- readPrimArray offsetRef offsetIndex
      let word8 = BSU.unsafeIndex bytes offset
      if word8 == minusSign
        then do
          writePrimArray offsetRef offsetIndex (offset + 1)
          fmap negate <$> parseUnsignedIntST scanner
        else
          if word8 == plusSign
            then writePrimArray offsetRef offsetIndex (offset + 1) >> parseUnsignedIntST scanner
            else parseUnsignedIntST scanner

nextIntegerST :: STScanner s %Many -> ST s Integer
{-# INLINE nextIntegerST #-}
nextIntegerST scanner@(STScanner bytes _ offsetRef) = do
  hasToken <- skipSpacesST scanner
  if not hasToken
    then inputError "nextIntegerST"
    else do
      offset <- readPrimArray offsetRef offsetIndex
      let word8 = BSU.unsafeIndex bytes offset
      if word8 == minusSign
        then do
          writePrimArray offsetRef offsetIndex (offset + 1)
          value <- requireParsed "nextIntegerST" <$> parseUnsignedIntegerST scanner
          pure (-value)
        else
          if word8 == plusSign
            then do
              writePrimArray offsetRef offsetIndex (offset + 1)
              requireParsed "nextIntegerST" <$> parseUnsignedIntegerST scanner
            else requireParsed "nextIntegerST" <$> parseUnsignedIntegerST scanner

nextWordST :: STScanner s %Many -> ST s Word
{-# INLINE nextWordST #-}
nextWordST scanner = do
  hasToken <- skipSpacesST scanner
  if not hasToken
    then inputError "nextWordST"
    else requireParsed "nextWordST" <$> parseUnsignedWordST scanner

nextByteStringST :: STScanner s %Many -> ST s BS.ByteString
{-# INLINE nextByteStringST #-}
nextByteStringST scanner =
  requireParsed "nextByteStringST" <$> maybeNextByteStringST scanner

maybeNextByteStringST :: STScanner s %Many -> ST s (Maybe BS.ByteString)
{-# INLINE maybeNextByteStringST #-}
maybeNextByteStringST scanner@(STScanner bytes len offsetRef) = do
  hasToken <- skipSpacesST scanner
  if not hasToken
    then pure Nothing
    else do
      start <- readPrimArray offsetRef offsetIndex
      let end = scanTokenEnd bytes len start
      writePrimArray offsetRef offsetIndex end
      pure (Just (BS.take (end - start) (BS.drop start bytes)))

nextCharST :: STScanner s %Many -> ST s Char
{-# INLINE nextCharST #-}
nextCharST scanner@(STScanner bytes _ offsetRef) = do
  hasToken <- skipSpacesST scanner
  if not hasToken
    then inputError "nextCharST"
    else do
      offset <- readPrimArray offsetRef offsetIndex
      writePrimArray offsetRef offsetIndex (offset + 1)
      pure (toEnum (fromIntegral (BSU.unsafeIndex bytes offset)))

nextIntListST :: Int -> STScanner s %Many -> ST s [Int]
{-# INLINE nextIntListST #-}
nextIntListST count scanner =
  go count []
  where
    go !remaining !values
      | remaining <= 0 = pure (reverse values)
      | otherwise = do
          value <- nextIntST scanner
          go (remaining - 1) (value : values)

parseUnsignedIntST :: STScanner s %Many -> ST s (Maybe Int)
{-# INLINE parseUnsignedIntST #-}
parseUnsignedIntST scanner@(STScanner bytes len offsetRef) = do
  offset <- readPrimArray offsetRef offsetIndex
  if offset < len
    then do
      let word8 = BSU.unsafeIndex bytes offset
      if isDigitWord8 word8
        then do
          let (!value, !end) =
                parseIntDigitsAt bytes len (offset + 1) (digitValue word8)
          writePrimArray offsetRef offsetIndex end
          pure (Just value)
        else pure Nothing
    else pure Nothing

parseUnsignedIntegerST :: STScanner s %Many -> ST s (Maybe Integer)
{-# INLINE parseUnsignedIntegerST #-}
parseUnsignedIntegerST scanner@(STScanner bytes len offsetRef) = do
  offset <- readPrimArray offsetRef offsetIndex
  if offset < len
    then do
      let word8 = BSU.unsafeIndex bytes offset
      if isDigitWord8 word8
        then do
          let (!value, !end) =
                parseIntegerDigitsAt bytes len (offset + 1) (fromIntegral (digitValue word8))
          writePrimArray offsetRef offsetIndex end
          pure (Just value)
        else pure Nothing
    else pure Nothing

parseUnsignedWordST :: STScanner s %Many -> ST s (Maybe Word)
{-# INLINE parseUnsignedWordST #-}
parseUnsignedWordST scanner@(STScanner bytes len offsetRef) = do
  offset <- readPrimArray offsetRef offsetIndex
  if offset < len
    then do
      let word8 = BSU.unsafeIndex bytes offset
      if isDigitWord8 word8
        then do
          let (!value, !end) =
                parseWordDigitsAt bytes len (offset + 1) (fromIntegral (digitValue word8))
          writePrimArray offsetRef offsetIndex end
          pure (Just value)
        else pure Nothing
    else pure Nothing

parseIntDigitsAt :: BS.ByteString -> Int -> Int -> Int -> (Int, Int)
{-# INLINE parseIntDigitsAt #-}
parseIntDigitsAt =
  parseDigitsAt (\acc word8 -> acc * 10 + digitValue word8)

parseIntegerDigitsAt :: BS.ByteString -> Int -> Int -> Integer -> (Integer, Int)
{-# INLINE parseIntegerDigitsAt #-}
parseIntegerDigitsAt =
  parseDigitsAt (\acc word8 -> acc * 10 + fromIntegral (digitValue word8))

parseWordDigitsAt :: BS.ByteString -> Int -> Int -> Word -> (Word, Int)
{-# INLINE parseWordDigitsAt #-}
parseWordDigitsAt =
  parseDigitsAt (\acc word8 -> acc * 10 + fromIntegral (digitValue word8))

parseDigitsAt :: (a -> Word8 -> a) -> BS.ByteString -> Int -> Int -> a -> (a, Int)
{-# INLINE parseDigitsAt #-}
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

skipSpacesST :: STScanner s %Many -> ST s Bool
{-# INLINE skipSpacesST #-}
skipSpacesST (STScanner bytes len offsetRef) = do
  offset <- readPrimArray offsetRef offsetIndex
  let !nextOffset = scanPastSpaces bytes len offset
  writePrimArray offsetRef offsetIndex nextOffset
  pure (nextOffset < len)

scanPastSpaces :: BS.ByteString -> Int -> Int -> Int
{-# INLINE scanPastSpaces #-}
scanPastSpaces bytes len =
  go
  where
    go !offset
      | offset < len && isSpaceWord8 (BSU.unsafeIndex bytes offset) = go (offset + 1)
      | otherwise = offset

scanTokenEnd :: BS.ByteString -> Int -> Int -> Int
{-# INLINE scanTokenEnd #-}
scanTokenEnd bytes len =
  go
  where
    go !offset
      | offset < len && not (isSpaceWord8 (BSU.unsafeIndex bytes offset)) = go (offset + 1)
      | otherwise = offset

digitValue :: Word8 -> Int
{-# INLINE digitValue #-}
digitValue word8 =
  fromIntegral (word8 - zeroChar)

isSpaceWord8 :: Word8 -> Bool
{-# INLINE isSpaceWord8 #-}
isSpaceWord8 word8 =
  word8 <= spaceChar

isDigitWord8 :: Word8 -> Bool
{-# INLINE isDigitWord8 #-}
isDigitWord8 word8 =
  zeroChar <= word8 && word8 <= nineChar

requireParsed :: String -> Maybe a -> a
{-# INLINE requireParsed #-}
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
