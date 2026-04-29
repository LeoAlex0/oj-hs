module Bundler.Output
  ( writeBundledSource
  ) where

import Control.Exception (SomeException, try)
import Bundler.Error (BundleError (OutputWriteFailed))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

writeBundledSource :: FilePath -> String -> IO (Either BundleError ())
writeBundledSource path source = do
  result <- try $ do
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path source
  case result of
    Left err -> pure (Left (OutputWriteFailed path (show (err :: SomeException))))
    Right () -> pure (Right ())
