module Bundler.Error
  ( BundleError (..)
  , renderBundleError
  ) where

import           Data.List (intercalate)

data BundleError
  = PackageDirectoryNotFound FilePath
  | CabalFileNotFound FilePath
  | MultipleCabalFiles FilePath [FilePath]
  | CabalLoadFailed FilePath String
  | NoExecutables FilePath
  | ExecutableNotFound String [String]
  | GhcSessionFailed String
  | GhcLoadFailed String
  | SymbolConflict String String
  | OutputWriteFailed FilePath String
  | SourceBundleFailed String
  deriving (Eq, Show)

renderBundleError :: BundleError -> String
renderBundleError err =
  case err of
    PackageDirectoryNotFound path ->
      "Package directory does not exist: " ++ path
    CabalFileNotFound path ->
      "No .cabal file found in package directory: " ++ path
    MultipleCabalFiles path cabalFiles ->
      "Expected one .cabal file in "
        ++ path
        ++ ", found: "
        ++ intercalate ", " cabalFiles
    CabalLoadFailed path message ->
      "Failed to load Cabal package description from " ++ path ++ ":\n" ++ message
    NoExecutables path ->
      "No executable stanzas found in package description: " ++ path
    ExecutableNotFound requested available ->
      "Unknown executable: "
        ++ requested
        ++ "\nAvailable executables: "
        ++ renderAvailable available
    GhcSessionFailed message ->
      "Failed to initialize GHC session:\n" ++ message
    GhcLoadFailed message ->
      "GHC failed to load the selected executable:\n" ++ message
    SymbolConflict left right ->
      "Generated symbol conflict between " ++ left ++ " and " ++ right
    OutputWriteFailed path message ->
      "Failed to write bundled source to " ++ path ++ ":\n" ++ message
    SourceBundleFailed message ->
      "Failed to generate bundled source:\n" ++ message

renderAvailable :: [String] -> String
renderAvailable []    = "(none)"
renderAvailable names = intercalate ", " names
