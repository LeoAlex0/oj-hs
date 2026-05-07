module Bundler.Env
  ( BundlerEnv (..)
  , loadBundleEnv
  ) where

import           Bundler.Cabal   (ExecutableInfo, PackageInfo, readPackageInfo,
                                  selectExecutable)
import           Bundler.Error   (BundleError)
import           Bundler.GHC     (LoadedGhcModules)
import           Bundler.Options (BundleOptions (..))

data BundlerEnv
  = BundlerEnv
      { envPackageInfo        :: PackageInfo
      , envSelectedExecutable :: ExecutableInfo
      , envLoadedModules      :: Maybe LoadedGhcModules
      }

loadBundleEnv :: BundleOptions -> IO (Either BundleError BundlerEnv)
loadBundleEnv options = do
  packageInfoResult <- readPackageInfo (optPackageDir options)
  pure $ do
    packageInfo <- packageInfoResult
    selectedExecutable <- selectExecutable (optExecutable options) packageInfo
    Right
      BundlerEnv
        { envPackageInfo = packageInfo
        , envSelectedExecutable = selectedExecutable
        , envLoadedModules = Nothing
        }
