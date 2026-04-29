module Main (main) where

import qualified Bundler
import Bundler.Error (renderBundleError)
import Bundler.Options (parseBundleOptions)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  options <- parseBundleOptions
  result <- Bundler.runBundler options
  case result of
    Right () -> pure ()
    Left err -> do
      hPutStrLn stderr (renderBundleError err)
      exitFailure
