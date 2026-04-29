module Bundler.Cabal
  ( ExecutableInfo (..)
  , PackageInfo (..)
  , readPackageInfo
  , selectExecutable
  ) where

import Control.Exception (SomeException, try)
import Data.List (sort)
import Distribution.Compiler (CompilerFlavor (GHC), PerCompilerFlavor (PerCompilerFlavor))
import Distribution.PackageDescription
  ( BuildInfo
  , Executable
  , buildInfo
  , condExecutables
  , defaultExtensions
  , executables
  , exeName
  , hsSourceDirs
  , libBuildInfo
  , library
  , modulePath
  , options
  , package
  , targetBuildDepends
  )
import Distribution.Package (pkgName)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.Dependency (depPkgName)
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Verbosity (silent)
import Bundler.Error (BundleError (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), normalise, takeExtension, takeFileName)

data PackageInfo = PackageInfo
  { packageRoot :: FilePath
  , packageCabalFile :: FilePath
  , packageName :: String
  , packageDisplayName :: String
  , packageLibrarySourceDirs :: [FilePath]
  , packageExecutables :: [ExecutableInfo]
  }
  deriving (Eq, Show)

data ExecutableInfo = ExecutableInfo
  { executableName :: String
  , executableMainPath :: FilePath
  , executableSourceDirs :: [FilePath]
  , executableDependencies :: [String]
  , executableDependencyPackageNames :: [String]
  , executableDefaultExtensions :: [String]
  , executableCompilerOptions :: [String]
  }
  deriving (Eq, Show)

readPackageInfo :: FilePath -> IO (Either BundleError PackageInfo)
readPackageInfo packageDir = do
  exists <- doesDirectoryExist packageDir
  if not exists
    then pure (Left (PackageDirectoryNotFound packageDir))
    else do
      cabalFiles <- findCabalFiles packageDir
      case cabalFiles of
        [] -> pure (Left (CabalFileNotFound packageDir))
        [_first, _second] -> pure (Left (MultipleCabalFiles packageDir (map takeFileName cabalFiles)))
        cabalFile : extraFiles ->
          if null extraFiles
            then readOneCabalFile packageDir cabalFile
            else pure (Left (MultipleCabalFiles packageDir (map takeFileName cabalFiles)))

selectExecutable :: Maybe String -> PackageInfo -> Either BundleError ExecutableInfo
selectExecutable requested packageInfo =
  case packageExecutables packageInfo of
    [] -> Left (NoExecutables (packageCabalFile packageInfo))
    executablesInPackage ->
      case requested of
        Nothing -> Right (head executablesInPackage)
        Just name ->
          case filter ((== name) . executableName) executablesInPackage of
            selected : _ -> Right selected
            [] -> Left (ExecutableNotFound name (map executableName executablesInPackage))

findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles packageDir = do
  entries <- listDirectory packageDir
  pure (sort [packageDir </> entry | entry <- entries, takeExtension entry == ".cabal"])

readOneCabalFile :: FilePath -> FilePath -> IO (Either BundleError PackageInfo)
readOneCabalFile packageDir cabalFile = do
  parsed <- try (readGenericPackageDescription silent cabalFile)
  case parsed of
    Left err -> pure (Left (CabalLoadFailed cabalFile (show (err :: SomeException))))
    Right genericPackageDescription -> do
      let packageDescription = flattenPackageDescription genericPackageDescription
          packageIdentifier = package packageDescription
          librarySourceDirs =
            case library packageDescription of
              Nothing -> []
              Just packageLibrary -> sourceDirectories packageDir (libBuildInfo packageLibrary)
          executableOrder =
            map (unUnqualComponentName . fst) (condExecutables genericPackageDescription)
          allExecutables =
            map (toExecutableInfo packageDir) $
              orderExecutables executableOrder (executables packageDescription)
      pure
        ( Right
            PackageInfo
              { packageRoot = packageDir
              , packageCabalFile = cabalFile
              , packageName = unPackageName (pkgName packageIdentifier)
              , packageDisplayName = prettyShow packageIdentifier
              , packageLibrarySourceDirs = librarySourceDirs
              , packageExecutables = allExecutables
              }
        )

toExecutableInfo :: FilePath -> Executable -> ExecutableInfo
toExecutableInfo packageDir executable =
  let info = buildInfo executable
      sourceDirs = sourceDirectories packageDir info
      mainPath =
        case sourceDirs of
          [] -> normalise (packageDir </> modulePath executable)
          sourceDir : _ -> normalise (sourceDir </> modulePath executable)
   in ExecutableInfo
        { executableName = unUnqualComponentName (exeName executable)
        , executableMainPath = mainPath
        , executableSourceDirs = sourceDirs
        , executableDependencies = map prettyShow (targetBuildDepends info)
        , executableDependencyPackageNames =
            map (unPackageName . depPkgName) (targetBuildDepends info)
        , executableDefaultExtensions = map prettyShow (defaultExtensions info)
        , executableCompilerOptions = ghcCompilerOptions info
        }

sourceDirectories :: FilePath -> BuildInfo -> [FilePath]
sourceDirectories packageDir info =
  case hsSourceDirs info of
    [] -> [normalise packageDir]
    dirs -> map (normalise . (packageDir </>) . getSymbolicPath) dirs

ghcCompilerOptions :: BuildInfo -> [String]
ghcCompilerOptions info =
  let PerCompilerFlavor ghcOptions _ = options info
   in ghcOptionsFor GHC ghcOptions

orderExecutables :: [String] -> [Executable] -> [Executable]
orderExecutables orderedNames unorderedExecutables =
  let ordered =
        [ executable
        | name <- orderedNames
        , executable <- unorderedExecutables
        , unUnqualComponentName (exeName executable) == name
        ]
      orderedNameSet = map (unUnqualComponentName . exeName) ordered
      remaining =
        [ executable
        | executable <- unorderedExecutables
        , unUnqualComponentName (exeName executable) `notElem` orderedNameSet
        ]
   in ordered ++ remaining

ghcOptionsFor :: CompilerFlavor -> [String] -> [String]
ghcOptionsFor GHC ghcOptions = ghcOptions
ghcOptionsFor _ ghcOptions = ghcOptions
