{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE LinearTypes  #-}

module System.IO.Fast
  ( Scanner
  , newScanner
  , scannerFromHandle
  , scannerFromByteString
  , nextInt
  , maybeNextInt
  , nextInteger
  , nextWord
  , nextByteString
  , maybeNextByteString
  , nextChar
  , nextIntList
  ) where

import           Control.Monad.Primitive  (PrimState)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Unsafe   as BSU
import           Data.Primitive.MutVar    (MutVar, newMutVar, readMutVar,
                                           writeMutVar)
import           Data.Primitive.PrimArray (MutablePrimArray, newPrimArray,
                                           readPrimArray, writePrimArray)
import           Data.Word                (Word, Word8)
import           GHC.Exts                 (Multiplicity (Many))
import           System.IO                (Handle, stdin)

data Scanner
  = ByteStringScanner !BS.ByteString !Int !(MutablePrimArray (PrimState IO) Int)
  | ChunkScanner !(MutVar (PrimState IO) BS.ByteString) !(MutablePrimArray (PrimState IO) Int) !(IO BS.ByteString)

offsetIndex :: Int
offsetIndex =
  0

lengthIndex :: Int
lengthIndex =
  1

eofIndex :: Int
eofIndex =
  2

defaultChunkSize :: Int
defaultChunkSize =
  32 * 1024

newScanner :: IO Scanner
newScanner =
  scannerFromHandle stdin

scannerFromHandle :: Handle -> IO Scanner
scannerFromHandle handle =
  scannerFromChunkReader (BS.hGetSome handle defaultChunkSize)

scannerFromByteString :: BS.ByteString -> IO Scanner
scannerFromByteString bytes = do
  offsetRef <- newPrimArray 1
  writePrimArray offsetRef offsetIndex 0
  pure (ByteStringScanner bytes (BS.length bytes) offsetRef)

scannerFromChunkReader :: IO BS.ByteString -> IO Scanner
scannerFromChunkReader readChunk = do
  bufferRef <- newMutVar BS.empty
  state <- newPrimArray 3
  writePrimArray state offsetIndex 0
  writePrimArray state lengthIndex 0
  writePrimArray state eofIndex 0
  pure (ChunkScanner bufferRef state readChunk)

nextInt :: Scanner %Many -> IO Int
{-# INLINE nextInt #-}
nextInt scanner =
  requireParsed "nextInt" <$> maybeNextInt scanner

maybeNextInt :: Scanner %Many -> IO (Maybe Int)
{-# INLINE maybeNextInt #-}
maybeNextInt scanner =
  case scanner of
    ByteStringScanner {} -> maybeNextIntBuffered scanner
    ChunkScanner {}      -> maybeNextIntChunked scanner

nextInteger :: Scanner %Many -> IO Integer
{-# INLINE nextInteger #-}
nextInteger scanner =
  case scanner of
    ByteStringScanner {} -> nextIntegerBuffered scanner
    ChunkScanner {}      -> nextIntegerChunked scanner

nextWord :: Scanner %Many -> IO Word
{-# INLINE nextWord #-}
nextWord scanner =
  case scanner of
    ByteStringScanner {} -> nextWordBuffered scanner
    ChunkScanner {}      -> nextWordChunked scanner

nextByteString :: Scanner %Many -> IO BS.ByteString
{-# INLINE nextByteString #-}
nextByteString scanner =
  requireParsed "nextByteString" <$> maybeNextByteString scanner

maybeNextByteString :: Scanner %Many -> IO (Maybe BS.ByteString)
{-# INLINE maybeNextByteString #-}
maybeNextByteString scanner =
  case scanner of
    ByteStringScanner {} -> maybeNextByteStringBuffered scanner
    ChunkScanner {}      -> maybeNextByteStringChunked scanner

nextChar :: Scanner %Many -> IO Char
{-# INLINE nextChar #-}
nextChar scanner =
  case scanner of
    ByteStringScanner {} -> nextCharBuffered scanner
    ChunkScanner {}      -> nextCharChunked scanner

nextIntList :: Int -> Scanner %Many -> IO [Int]
{-# INLINE nextIntList #-}
nextIntList count scanner =
  go count []
  where
    go !remaining !values
      | remaining <= 0 = pure (reverse values)
      | otherwise = do
          value <- nextInt scanner
          go (remaining - 1) (value : values)

maybeNextIntBuffered :: Scanner %Many -> IO (Maybe Int)
{-# INLINE maybeNextIntBuffered #-}
maybeNextIntBuffered scanner@(ByteStringScanner bytes _ offsetRef) = do
  hasToken <- skipSpacesBuffered scanner
  if not hasToken
    then pure Nothing
    else do
      offset <- readPrimArray offsetRef offsetIndex
      let !word8 = BSU.unsafeIndex bytes offset
      if word8 == minusSign
        then do
          writePrimArray offsetRef offsetIndex (offset + 1)
          fmap negate <$> parseUnsignedIntBuffered scanner
        else
          if word8 == plusSign
            then do
              writePrimArray offsetRef offsetIndex (offset + 1)
              parseUnsignedIntBuffered scanner
            else parseUnsignedIntBuffered scanner
maybeNextIntBuffered _ =
  inputError "maybeNextInt"

nextIntegerBuffered :: Scanner %Many -> IO Integer
{-# INLINE nextIntegerBuffered #-}
nextIntegerBuffered scanner@(ByteStringScanner bytes _ offsetRef) = do
  hasToken <- skipSpacesBuffered scanner
  if not hasToken
    then inputError "nextInteger"
    else do
      offset <- readPrimArray offsetRef offsetIndex
      let !word8 = BSU.unsafeIndex bytes offset
      if word8 == minusSign
        then do
          writePrimArray offsetRef offsetIndex (offset + 1)
          value <- requireParsed "nextInteger" <$> parseUnsignedIntegerBuffered scanner
          pure (-value)
        else
          if word8 == plusSign
            then do
              writePrimArray offsetRef offsetIndex (offset + 1)
              requireParsed "nextInteger" <$> parseUnsignedIntegerBuffered scanner
            else requireParsed "nextInteger" <$> parseUnsignedIntegerBuffered scanner
nextIntegerBuffered _ =
  inputError "nextInteger"

nextWordBuffered :: Scanner %Many -> IO Word
{-# INLINE nextWordBuffered #-}
nextWordBuffered scanner = do
  hasToken <- skipSpacesBuffered scanner
  if not hasToken
    then inputError "nextWord"
    else requireParsed "nextWord" <$> parseUnsignedWordBuffered scanner

maybeNextByteStringBuffered :: Scanner %Many -> IO (Maybe BS.ByteString)
{-# INLINE maybeNextByteStringBuffered #-}
maybeNextByteStringBuffered scanner@(ByteStringScanner bytes len offsetRef) = do
  hasToken <- skipSpacesBuffered scanner
  if not hasToken
    then pure Nothing
    else do
      start <- readPrimArray offsetRef offsetIndex
      let !end = scanTokenEnd bytes len start
      writePrimArray offsetRef offsetIndex end
      pure (Just (sliceByteString bytes start end))
maybeNextByteStringBuffered _ =
  inputError "maybeNextByteString"

nextCharBuffered :: Scanner %Many -> IO Char
{-# INLINE nextCharBuffered #-}
nextCharBuffered scanner@(ByteStringScanner bytes _ offsetRef) = do
  hasToken <- skipSpacesBuffered scanner
  if not hasToken
    then inputError "nextChar"
    else do
      offset <- readPrimArray offsetRef offsetIndex
      writePrimArray offsetRef offsetIndex (offset + 1)
      pure (toEnum (fromIntegral (BSU.unsafeIndex bytes offset)))
nextCharBuffered _ =
  inputError "nextChar"

parseUnsignedIntBuffered :: Scanner %Many -> IO (Maybe Int)
{-# INLINE parseUnsignedIntBuffered #-}
parseUnsignedIntBuffered (ByteStringScanner bytes len offsetRef) = do
  offset <- readPrimArray offsetRef offsetIndex
  if offset < len
    then do
      let !word8 = BSU.unsafeIndex bytes offset
      if isDigitWord8 word8
        then do
          let (!value, !end) =
                parseIntDigitsAt bytes len (offset + 1) (digitValue word8)
          writePrimArray offsetRef offsetIndex end
          pure (Just value)
        else pure Nothing
    else pure Nothing
parseUnsignedIntBuffered _ =
  pure Nothing

parseUnsignedIntegerBuffered :: Scanner %Many -> IO (Maybe Integer)
{-# INLINE parseUnsignedIntegerBuffered #-}
parseUnsignedIntegerBuffered (ByteStringScanner bytes len offsetRef) = do
  offset <- readPrimArray offsetRef offsetIndex
  if offset < len
    then do
      let !word8 = BSU.unsafeIndex bytes offset
      if isDigitWord8 word8
        then do
          let (!value, !end) =
                parseIntegerDigitsAt bytes len (offset + 1) (fromIntegral (digitValue word8))
          writePrimArray offsetRef offsetIndex end
          pure (Just value)
        else pure Nothing
    else pure Nothing
parseUnsignedIntegerBuffered _ =
  pure Nothing

parseUnsignedWordBuffered :: Scanner %Many -> IO (Maybe Word)
{-# INLINE parseUnsignedWordBuffered #-}
parseUnsignedWordBuffered (ByteStringScanner bytes len offsetRef) = do
  offset <- readPrimArray offsetRef offsetIndex
  if offset < len
    then do
      let !word8 = BSU.unsafeIndex bytes offset
      if isDigitWord8 word8
        then do
          let (!value, !end) =
                parseWordDigitsAt bytes len (offset + 1) (fromIntegral (digitValue word8))
          writePrimArray offsetRef offsetIndex end
          pure (Just value)
        else pure Nothing
    else pure Nothing
parseUnsignedWordBuffered _ =
  pure Nothing

skipSpacesBuffered :: Scanner %Many -> IO Bool
{-# INLINE skipSpacesBuffered #-}
skipSpacesBuffered (ByteStringScanner bytes len offsetRef) = do
  offset <- readPrimArray offsetRef offsetIndex
  let !nextOffset = scanPastSpaces bytes len offset
  writePrimArray offsetRef offsetIndex nextOffset
  pure (nextOffset < len)
skipSpacesBuffered _ =
  pure False

maybeNextIntChunked :: Scanner %Many -> IO (Maybe Int)
{-# INLINE maybeNextIntChunked #-}
maybeNextIntChunked scanner = do
  hasToken <- skipSpacesChunked scanner
  if not hasToken
    then pure Nothing
    else do
      word8 <- peekChunked scanner
      if word8 == minusSign
        then do
          advanceChunked scanner 1
          fmap negate <$> parseUnsignedIntChunked scanner
        else
          if word8 == plusSign
            then do
              advanceChunked scanner 1
              parseUnsignedIntChunked scanner
            else parseUnsignedIntChunked scanner

nextIntegerChunked :: Scanner %Many -> IO Integer
{-# INLINE nextIntegerChunked #-}
nextIntegerChunked scanner = do
  hasToken <- skipSpacesChunked scanner
  if not hasToken
    then inputError "nextInteger"
    else do
      word8 <- peekChunked scanner
      if word8 == minusSign
        then do
          advanceChunked scanner 1
          value <- requireParsed "nextInteger" <$> parseUnsignedIntegerChunked scanner
          pure (-value)
        else
          if word8 == plusSign
            then do
              advanceChunked scanner 1
              requireParsed "nextInteger" <$> parseUnsignedIntegerChunked scanner
            else requireParsed "nextInteger" <$> parseUnsignedIntegerChunked scanner

nextWordChunked :: Scanner %Many -> IO Word
{-# INLINE nextWordChunked #-}
nextWordChunked scanner = do
  hasToken <- skipSpacesChunked scanner
  if not hasToken
    then inputError "nextWord"
    else requireParsed "nextWord" <$> parseUnsignedWordChunked scanner

maybeNextByteStringChunked :: Scanner %Many -> IO (Maybe BS.ByteString)
{-# INLINE maybeNextByteStringChunked #-}
maybeNextByteStringChunked scanner = do
  hasToken <- skipSpacesChunked scanner
  if hasToken
    then Just <$> nextTokenChunked scanner []
    else pure Nothing

nextCharChunked :: Scanner %Many -> IO Char
{-# INLINE nextCharChunked #-}
nextCharChunked scanner = do
  hasToken <- skipSpacesChunked scanner
  if not hasToken
    then inputError "nextChar"
    else do
      word8 <- peekChunked scanner
      advanceChunked scanner 1
      pure (toEnum (fromIntegral word8))

parseUnsignedIntChunked :: Scanner %Many -> IO (Maybe Int)
{-# INLINE parseUnsignedIntChunked #-}
parseUnsignedIntChunked scanner = do
  hasBuffer <- ensureChunkedBuffer scanner
  if not hasBuffer
    then pure Nothing
    else do
      word8 <- peekChunked scanner
      if isDigitWord8 word8
        then do
          advanceChunked scanner 1
          Just <$> parseDigitsChunked (\acc digit -> acc * 10 + digitValue digit) scanner (digitValue word8)
        else pure Nothing

parseUnsignedIntegerChunked :: Scanner %Many -> IO (Maybe Integer)
{-# INLINE parseUnsignedIntegerChunked #-}
parseUnsignedIntegerChunked scanner = do
  hasBuffer <- ensureChunkedBuffer scanner
  if not hasBuffer
    then pure Nothing
    else do
      word8 <- peekChunked scanner
      if isDigitWord8 word8
        then do
          advanceChunked scanner 1
          Just <$> parseDigitsChunked (\acc digit -> acc * 10 + fromIntegral (digitValue digit)) scanner (fromIntegral (digitValue word8))
        else pure Nothing

parseUnsignedWordChunked :: Scanner %Many -> IO (Maybe Word)
{-# INLINE parseUnsignedWordChunked #-}
parseUnsignedWordChunked scanner = do
  hasBuffer <- ensureChunkedBuffer scanner
  if not hasBuffer
    then pure Nothing
    else do
      word8 <- peekChunked scanner
      if isDigitWord8 word8
        then do
          advanceChunked scanner 1
          Just <$> parseDigitsChunked (\acc digit -> acc * 10 + fromIntegral (digitValue digit)) scanner (fromIntegral (digitValue word8))
        else pure Nothing

parseDigitsChunked :: (a -> Word8 -> a) -> Scanner %Many -> a -> IO a
{-# INLINE parseDigitsChunked #-}
parseDigitsChunked appendDigit scanner !acc = do
  hasBuffer <- ensureChunkedBuffer scanner
  if not hasBuffer
    then pure acc
    else do
      (bytes, offset, len) <- chunkedBufferState scanner
      let (!value, !end) = parseDigitsAt appendDigit bytes len offset acc
      writeChunkedOffset scanner end
      if end < len
        then pure value
        else parseDigitsChunked appendDigit scanner value

nextTokenChunked :: Scanner %Many -> [BS.ByteString] -> IO BS.ByteString
{-# INLINE nextTokenChunked #-}
nextTokenChunked scanner pieces = do
  (bytes, start, len) <- chunkedBufferState scanner
  let !end = scanTokenEnd bytes len start
      !piece = sliceByteString bytes start end
  writeChunkedOffset scanner end
  if end < len
    then pure (concatTokenPieces piece pieces)
    else do
      hasMore <- ensureChunkedBuffer scanner
      if not hasMore
        then pure (concatTokenPieces piece pieces)
        else do
          word8 <- peekChunked scanner
          if isSpaceWord8 word8
            then pure (concatTokenPieces piece pieces)
            else nextTokenChunked scanner (piece : pieces)

skipSpacesChunked :: Scanner %Many -> IO Bool
{-# INLINE skipSpacesChunked #-}
skipSpacesChunked scanner = do
  hasBuffer <- ensureChunkedBuffer scanner
  if not hasBuffer
    then pure False
    else do
      (bytes, offset, len) <- chunkedBufferState scanner
      let !nextOffset = scanPastSpaces bytes len offset
      writeChunkedOffset scanner nextOffset
      if nextOffset < len
        then pure True
        else skipSpacesChunked scanner

ensureChunkedBuffer :: Scanner %Many -> IO Bool
{-# INLINE ensureChunkedBuffer #-}
ensureChunkedBuffer scanner@(ChunkScanner _ state _) = do
  offset <- readPrimArray state offsetIndex
  len <- readPrimArray state lengthIndex
  if offset < len
    then pure True
    else refillChunkedBuffer scanner
ensureChunkedBuffer _ =
  pure False

refillChunkedBuffer :: Scanner %Many -> IO Bool
{-# INLINE refillChunkedBuffer #-}
refillChunkedBuffer (ChunkScanner bufferRef state readChunk) = do
  eof <- readPrimArray state eofIndex
  if eof /= 0
    then pure False
    else do
      chunk <- readChunk
      if BS.null chunk
        then do
          writeMutVar bufferRef BS.empty
          writePrimArray state offsetIndex 0
          writePrimArray state lengthIndex 0
          writePrimArray state eofIndex 1
          pure False
        else do
          writeMutVar bufferRef chunk
          writePrimArray state offsetIndex 0
          writePrimArray state lengthIndex (BS.length chunk)
          pure True
refillChunkedBuffer _ =
  pure False

chunkedBufferState :: Scanner %Many -> IO (BS.ByteString, Int, Int)
{-# INLINE chunkedBufferState #-}
chunkedBufferState (ChunkScanner bufferRef state _) = do
  bytes <- readMutVar bufferRef
  offset <- readPrimArray state offsetIndex
  len <- readPrimArray state lengthIndex
  pure (bytes, offset, len)
chunkedBufferState _ =
  inputError "chunkedBufferState"

peekChunked :: Scanner %Many -> IO Word8
{-# INLINE peekChunked #-}
peekChunked scanner = do
  (bytes, offset, _) <- chunkedBufferState scanner
  pure (BSU.unsafeIndex bytes offset)

advanceChunked :: Scanner %Many -> Int -> IO ()
{-# INLINE advanceChunked #-}
advanceChunked scanner count = do
  (_, offset, _) <- chunkedBufferState scanner
  writeChunkedOffset scanner (offset + count)

writeChunkedOffset :: Scanner %Many -> Int -> IO ()
{-# INLINE writeChunkedOffset #-}
writeChunkedOffset (ChunkScanner _ state _) offset =
  writePrimArray state offsetIndex offset
writeChunkedOffset _ _ =
  pure ()

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
          let !word8 = BSU.unsafeIndex bytes offset
           in if isDigitWord8 word8
                then go (offset + 1) (appendDigit acc word8)
                else (acc, offset)
      | otherwise = (acc, offset)

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

concatTokenPieces :: BS.ByteString -> [BS.ByteString] -> BS.ByteString
{-# INLINE concatTokenPieces #-}
concatTokenPieces piece [] =
  piece
concatTokenPieces piece pieces =
  BS.concat (reverse (piece : pieces))

sliceByteString :: BS.ByteString -> Int -> Int -> BS.ByteString
{-# INLINE sliceByteString #-}
sliceByteString bytes start end =
  BS.take (end - start) (BS.drop start bytes)

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
