module Bundler
  ( runBundler
  ) where

import Bundler.Env (envPackageInfo, envSelectedExecutable, loadBundleEnv)
import Bundler.Error (BundleError)
import Bundler.GHC (loadExecutableModules)
import Bundler.Options (BundleOptions (..))
import Bundler.Output (writeBundledSource)
import Bundler.SourceBundle (generateSourceBundle)

runBundler :: BundleOptions -> IO (Either BundleError ())
runBundler options = do
  envResult <- loadBundleEnv options
  case envResult of
    Left err -> pure (Left err)
    Right env -> do
      loadedModulesResult <-
        loadExecutableModules
          (envPackageInfo env)
          (envSelectedExecutable env)
      case loadedModulesResult of
        Left err -> pure (Left err)
        Right loadedModules -> do
          sourceResult <-
            generateSourceBundle
              (envPackageInfo env)
              (envSelectedExecutable env)
              loadedModules
          case sourceResult of
            Left err -> pure (Left err)
            Right source -> writeBundledSource (optOutput options) source
