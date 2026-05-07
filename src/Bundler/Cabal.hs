module Bundler.Cabal
  ( ExecutableInfo (..)
  , PackageInfo (..)
  , readPackageInfo
  , selectExecutable
  ) where

import           Bundler.Error                                 (BundleError (..))
import           Control.Exception                             (SomeException,
                                                                try)
import           Control.Monad                                 (filterM)
import           Data.List                                     (sort)
import           Distribution.Compiler                         (CompilerFlavor (GHC),
                                                                PerCompilerFlavor (PerCompilerFlavor))
import           Distribution.Package                          (pkgName,
                                                                pkgVersion)
import           Distribution.PackageDescription               (BuildInfo,
                                                                Executable,
                                                                buildInfo,
                                                                condExecutables,
                                                                cppOptions,
                                                                defaultExtensions,
                                                                exeName,
                                                                executables,
                                                                hsSourceDirs,
                                                                includeDirs,
                                                                libBuildInfo,
                                                                library,
                                                                modulePath,
                                                                oldExtensions,
                                                                options,
                                                                package,
                                                                targetBuildDepends)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Pretty                           (prettyShow)
import           Distribution.Simple.PackageDescription        (readGenericPackageDescription)
import           Distribution.Types.Dependency                 (depPkgName)
import           Distribution.Types.PackageName                (unPackageName)
import           Distribution.Types.UnqualComponentName        (unUnqualComponentName)
import           Distribution.Types.Version                    (versionNumbers)
import           Distribution.Utils.Path                       (getSymbolicPath)
import           Distribution.Verbosity                        (silent)
import           System.Directory                              (doesDirectoryExist,
                                                                doesFileExist,
                                                                listDirectory)
import           System.FilePath                               (normalise,
                                                                takeExtension,
                                                                takeFileName,
                                                                (</>))

data PackageInfo
  = PackageInfo
      { packageRoot                          :: FilePath
      , packageCabalFile                     :: FilePath
      , packageName                          :: String
      , packageDisplayName                   :: String
      , packagePathsModuleName               :: String
      , packageVersionNumbers                :: [Int]
      , packageLibrarySourceDirs             :: [FilePath]
      , packageLibraryDependencyPackageNames :: [String]
      , packageLibraryDefaultExtensions      :: [String]
      , packageLibraryCompilerOptions        :: [String]
      , packageExecutables                   :: [ExecutableInfo]
      }
  deriving (Eq, Show)

data ExecutableInfo
  = ExecutableInfo
      { executableName                   :: String
      , executableMainPath               :: FilePath
      , executableSourceDirs             :: [FilePath]
      , executableDependencies           :: [String]
      , executableDependencyPackageNames :: [String]
      , executableDefaultExtensions      :: [String]
      , executableCompilerOptions        :: [String]
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
          packageNameValue = unPackageName (pkgName packageIdentifier)
          librarySourceDirs =
            case library packageDescription of
              Nothing -> []
              Just packageLibrary -> sourceDirectories packageDir (libBuildInfo packageLibrary)
          libraryDependencyPackageNames =
            case library packageDescription of
              Nothing -> []
              Just packageLibrary -> dependencyPackageNames (libBuildInfo packageLibrary)
          libraryDefaultExtensions =
            case library packageDescription of
              Nothing -> []
              Just packageLibrary -> buildInfoDefaultExtensions (libBuildInfo packageLibrary)
          libraryCompilerOptions =
            case library packageDescription of
              Nothing -> []
              Just packageLibrary -> buildInfoCompilerOptions packageDir (libBuildInfo packageLibrary)
          executableOrder =
            map (unUnqualComponentName . fst) (condExecutables genericPackageDescription)
      allExecutables <-
        mapM (toExecutableInfo packageDir) $
          orderExecutables executableOrder (executables packageDescription)
      pure
        ( Right
            PackageInfo
              { packageRoot = packageDir
              , packageCabalFile = cabalFile
              , packageName = packageNameValue
              , packageDisplayName = prettyShow packageIdentifier
              , packagePathsModuleName = pathsModuleNameForPackage packageNameValue
              , packageVersionNumbers = versionNumbers (pkgVersion packageIdentifier)
              , packageLibrarySourceDirs = librarySourceDirs
              , packageLibraryDependencyPackageNames = libraryDependencyPackageNames
              , packageLibraryDefaultExtensions = libraryDefaultExtensions
              , packageLibraryCompilerOptions = libraryCompilerOptions
              , packageExecutables = allExecutables
              }
        )

toExecutableInfo :: FilePath -> Executable -> IO ExecutableInfo
toExecutableInfo packageDir executable = do
  mainPath <- resolveExecutableMainPath packageDir sourceDirs (modulePath executable)
  pure
    ExecutableInfo
      { executableName = unUnqualComponentName (exeName executable)
      , executableMainPath = mainPath
      , executableSourceDirs = sourceDirs
      , executableDependencies = map prettyShow (targetBuildDepends info)
      , executableDependencyPackageNames = dependencyPackageNames info
      , executableDefaultExtensions = buildInfoDefaultExtensions info
      , executableCompilerOptions = buildInfoCompilerOptions packageDir info
      }
  where
    info = buildInfo executable
    sourceDirs = sourceDirectories packageDir info

resolveExecutableMainPath :: FilePath -> [FilePath] -> FilePath -> IO FilePath
resolveExecutableMainPath packageDir sourceDirs mainFile = do
  let candidateDirs =
        case sourceDirs of
          []   -> [normalise packageDir]
          dirs -> dirs
      candidates = map (normalise . (</> mainFile)) candidateDirs
  existing <- filterM doesFileExist candidates
  pure
    ( case existing of
        path : _ -> path
        [] ->
          case candidates of
            path : _ -> path
            []       -> normalise (packageDir </> mainFile)
    )

dependencyPackageNames :: BuildInfo -> [String]
dependencyPackageNames info =
  map (unPackageName . depPkgName) (targetBuildDepends info)

pathsModuleNameForPackage :: String -> String
pathsModuleNameForPackage packageNameValue =
  "Paths_" ++ map packageNameModuleChar packageNameValue

packageNameModuleChar :: Char -> Char
packageNameModuleChar '-'  = '_'
packageNameModuleChar char = char

sourceDirectories :: FilePath -> BuildInfo -> [FilePath]
sourceDirectories packageDir info =
  case hsSourceDirs info of
    []   -> [normalise packageDir]
    dirs -> map (normalise . (packageDir </>) . getSymbolicPath) dirs

buildInfoDefaultExtensions :: BuildInfo -> [String]
buildInfoDefaultExtensions info =
  map prettyShow (defaultExtensions info ++ oldExtensions info)

buildInfoCompilerOptions :: FilePath -> BuildInfo -> [String]
buildInfoCompilerOptions packageDir info =
  ghcCompilerOptions info
    ++ cppOptions info
    ++ map (("-I" ++) . normalise . (packageDir </>)) (includeDirs info)

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
ghcOptionsFor _ ghcOptions   = ghcOptions
