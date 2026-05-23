module Bundler.Output
  ( writeBundledSource
  ) where

import           Bundler.Error     (BundleError (OutputWriteFailed))
import           Control.Exception (SomeException, try)
import           System.Directory  (createDirectoryIfMissing)
import           System.FilePath   (takeDirectory)

writeBundledSource :: Maybe FilePath -> String -> IO (Either BundleError ())
writeBundledSource Nothing source = do
  result <- try (putStr source)
  case result of
    Left err -> pure (Left (OutputWriteFailed "<stdout>" (show (err :: SomeException))))
    Right () -> pure (Right ())
writeBundledSource (Just path) source = do
  result <- try $ do
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path source
  case result of
    Left err -> pure (Left (OutputWriteFailed path (show (err :: SomeException))))
    Right () -> pure (Right ())
