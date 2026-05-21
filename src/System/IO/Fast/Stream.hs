{-# LANGUAGE BangPatterns #-}

module System.IO.Fast.Stream
  ( StreamScanner
  , newStreamScanner
  , streamScannerFromHandle
  , streamScannerFromHandleWithChunkSize
  , streamScannerFromChunks
  , defaultStreamChunkSize
  , nextIntIO
  , maybeNextIntIO
  , nextIntegerIO
  , nextWordIO
  , nextByteStringIO
  , maybeNextByteStringIO
  , nextCharIO
  , nextIntListIO
  ) where

import qualified Data.ByteString as BS
import           Data.IORef      (IORef, modifyIORef', newIORef, readIORef,
                                  writeIORef)
import           Data.Word       (Word, Word8)
import           System.IO       (Handle, stdin)

data StreamScanner
  = StreamScanner !(IORef BS.ByteString) !(IO BS.ByteString)

defaultStreamChunkSize :: Int
defaultStreamChunkSize =
  32 * 1024

newStreamScanner :: IO StreamScanner
newStreamScanner =
  streamScannerFromHandle stdin

streamScannerFromHandle :: Handle -> IO StreamScanner
streamScannerFromHandle =
  streamScannerFromHandleWithChunkSize defaultStreamChunkSize

streamScannerFromHandleWithChunkSize :: Int -> Handle -> IO StreamScanner
streamScannerFromHandleWithChunkSize chunkSize handle =
  streamScannerFromChunkReader (BS.hGetSome handle (max 1 chunkSize))

streamScannerFromChunks :: [BS.ByteString] -> IO StreamScanner
streamScannerFromChunks chunks = do
  chunksRef <- newIORef chunks
  streamScannerFromChunkReader $ do
    remainingChunks <- readIORef chunksRef
    case remainingChunks of
      []              -> pure BS.empty
      chunk : chunks' -> writeIORef chunksRef chunks' >> pure chunk

streamScannerFromChunkReader :: IO BS.ByteString -> IO StreamScanner
streamScannerFromChunkReader readChunk = do
  bufferRef <- newIORef BS.empty
  pure (StreamScanner bufferRef readChunk)

nextIntIO :: StreamScanner -> IO Int
nextIntIO scanner =
  requireParsed "nextIntIO" <$> maybeNextIntIO scanner

maybeNextIntIO :: StreamScanner -> IO (Maybe Int)
maybeNextIntIO scanner@(StreamScanner bufferRef _) = do
  hasToken <- skipSpacesStream scanner
  if not hasToken
    then pure Nothing
    else do
      bytes <- readIORef bufferRef
      case BS.uncons bytes of
        Just (word8, rest)
          | word8 == minusSign -> do
              writeIORef bufferRef rest
              fmap negate <$> parseUnsignedIntStream scanner
          | word8 == plusSign -> do
              writeIORef bufferRef rest
              parseUnsignedIntStream scanner
        _ ->
          parseUnsignedIntStream scanner

nextIntegerIO :: StreamScanner -> IO Integer
nextIntegerIO scanner@(StreamScanner bufferRef _) = do
  hasToken <- skipSpacesStream scanner
  if not hasToken
    then inputError "nextIntegerIO"
    else do
      bytes <- readIORef bufferRef
      case BS.uncons bytes of
        Just (word8, rest)
          | word8 == minusSign -> do
              writeIORef bufferRef rest
              value <- requireParsed "nextIntegerIO" <$> parseUnsignedIntegerStream scanner
              pure (-value)
          | word8 == plusSign -> do
              writeIORef bufferRef rest
              requireParsed "nextIntegerIO" <$> parseUnsignedIntegerStream scanner
        _ ->
          requireParsed "nextIntegerIO" <$> parseUnsignedIntegerStream scanner

nextWordIO :: StreamScanner -> IO Word
nextWordIO scanner = do
  hasToken <- skipSpacesStream scanner
  if not hasToken
    then inputError "nextWordIO"
    else requireParsed "nextWordIO" <$> parseUnsignedWordStream scanner

nextByteStringIO :: StreamScanner -> IO BS.ByteString
nextByteStringIO scanner =
  requireParsed "nextByteStringIO" <$> maybeNextByteStringIO scanner

maybeNextByteStringIO :: StreamScanner -> IO (Maybe BS.ByteString)
maybeNextByteStringIO scanner = do
  hasToken <- skipSpacesStream scanner
  if hasToken
    then Just <$> nextTokenStream scanner []
    else pure Nothing

nextCharIO :: StreamScanner -> IO Char
nextCharIO scanner@(StreamScanner bufferRef _) = do
  hasToken <- skipSpacesStream scanner
  if not hasToken
    then inputError "nextCharIO"
    else do
      bytes <- readIORef bufferRef
      case BS.uncons bytes of
        Just (word8, remaining) -> do
          writeIORef bufferRef remaining
          pure (toEnum (fromIntegral word8))
        Nothing ->
          inputError "nextCharIO"

nextIntListIO :: Int -> StreamScanner -> IO [Int]
nextIntListIO count scanner =
  go count []
  where
    go !remaining !values
      | remaining <= 0 = pure (reverse values)
      | otherwise = do
          value <- nextIntIO scanner
          go (remaining - 1) (value : values)

parseUnsignedIntStream :: StreamScanner -> IO (Maybe Int)
parseUnsignedIntStream scanner@(StreamScanner bufferRef _) = do
  bytes <- readIORef bufferRef
  case BS.uncons bytes of
    Just (word8, remaining)
      | isDigitWord8 word8 -> do
          writeIORef bufferRef remaining
          Just <$> parseIntDigitsStream scanner (digitValue word8)
    Nothing -> do
      hasMore <- readMoreStream scanner
      if hasMore
        then parseUnsignedIntStream scanner
        else pure Nothing
    _ -> pure Nothing

parseUnsignedIntegerStream :: StreamScanner -> IO (Maybe Integer)
parseUnsignedIntegerStream scanner@(StreamScanner bufferRef _) = do
  bytes <- readIORef bufferRef
  case BS.uncons bytes of
    Just (word8, remaining)
      | isDigitWord8 word8 -> do
          writeIORef bufferRef remaining
          Just <$> parseIntegerDigitsStream scanner (fromIntegral (digitValue word8))
    Nothing -> do
      hasMore <- readMoreStream scanner
      if hasMore
        then parseUnsignedIntegerStream scanner
        else pure Nothing
    _ -> pure Nothing

parseUnsignedWordStream :: StreamScanner -> IO (Maybe Word)
parseUnsignedWordStream scanner@(StreamScanner bufferRef _) = do
  bytes <- readIORef bufferRef
  case BS.uncons bytes of
    Just (word8, remaining)
      | isDigitWord8 word8 -> do
          writeIORef bufferRef remaining
          Just <$> parseWordDigitsStream scanner (fromIntegral (digitValue word8))
    Nothing -> do
      hasMore <- readMoreStream scanner
      if hasMore
        then parseUnsignedWordStream scanner
        else pure Nothing
    _ -> pure Nothing

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

parseIntDigitsStream :: StreamScanner -> Int -> IO Int
parseIntDigitsStream =
  parseDigitsStream parseIntDigits

parseIntegerDigitsStream :: StreamScanner -> Integer -> IO Integer
parseIntegerDigitsStream =
  parseDigitsStream parseIntegerDigits

parseWordDigitsStream :: StreamScanner -> Word -> IO Word
parseWordDigitsStream =
  parseDigitsStream parseWordDigits

parseDigitsStream ::
     (a -> BS.ByteString -> (a, BS.ByteString))
  -> StreamScanner
  -> a
  -> IO a
parseDigitsStream parseDigits scanner@(StreamScanner bufferRef _) !acc = do
  bytes <- readIORef bufferRef
  let (!value, remaining) = parseDigits acc bytes
  if BS.null remaining
    then do
      writeIORef bufferRef BS.empty
      hasMore <- readMoreStream scanner
      if hasMore
        then do
          nextBytes <- readIORef bufferRef
          case BS.uncons nextBytes of
            Just (word8, _)
              | isDigitWord8 word8 -> parseDigitsStream parseDigits scanner value
            _ -> pure value
        else pure value
    else do
      writeIORef bufferRef remaining
      pure value

nextTokenStream :: StreamScanner -> [BS.ByteString] -> IO BS.ByteString
nextTokenStream scanner@(StreamScanner bufferRef _) pieces = do
  bytes <- readIORef bufferRef
  let (tokenPart, remaining) = BS.span (not . isSpaceWord8) bytes
  if BS.null remaining
    then do
      writeIORef bufferRef BS.empty
      hasMore <- readMoreStream scanner
      if hasMore
        then nextTokenStream scanner (tokenPart : pieces)
        else pure (BS.concat (reverse (tokenPart : pieces)))
    else do
      writeIORef bufferRef remaining
      pure (BS.concat (reverse (tokenPart : pieces)))

skipSpacesStream :: StreamScanner -> IO Bool
skipSpacesStream scanner@(StreamScanner bufferRef _) = do
  bytes <- readIORef bufferRef
  let remaining = skipSpaces bytes
  if BS.null remaining
    then do
      writeIORef bufferRef BS.empty
      hasMore <- readMoreStream scanner
      if hasMore
        then skipSpacesStream scanner
        else pure False
    else do
      writeIORef bufferRef remaining
      pure True

readMoreStream :: StreamScanner -> IO Bool
readMoreStream (StreamScanner bufferRef readChunk) = do
  chunk <- readChunk
  if BS.null chunk
    then pure False
    else do
      let appendChunk bytes
            | BS.null bytes = chunk
            | otherwise     = bytes <> chunk
      modifyIORef' bufferRef appendChunk
      pure True

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
  error ("System.IO.Fast.Stream." ++ name ++ ": input exhausted or malformed")

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
