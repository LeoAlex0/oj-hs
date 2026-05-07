module Bundler.GHC
  ( GhcConfig (..)
  , LoadedGhcModules (..)
  , LoadedModule (..)
  , loadExecutableModules
  , resolveGhcLibDir
  ) where

import           Bundler.Cabal            (ExecutableInfo (..),
                                           PackageInfo (..))
import           Bundler.Error            (BundleError (..))
import           Control.Exception        (SomeException, bracket, try)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Char                (isSpace)
import           Data.IORef               (IORef, modifyIORef', newIORef,
                                           readIORef)
import           Data.List                (intercalate, nub)
import           Data.Maybe               (isJust, mapMaybe)
import           GHC                      (Ghc, LoadHowMuch (LoadAllTargets),
                                           Located, ModSummary, ModuleGraph,
                                           RenamedSource,
                                           SuccessFlag (Failed, Succeeded),
                                           TypecheckedSource, getModuleGraph,
                                           getSession, getSessionDynFlags,
                                           guessTarget, load, modInfoRdrEnv,
                                           moduleInfo, moduleNameString,
                                           ms_location, ms_mod_name,
                                           parseDynamicFlags, parseModule,
                                           runGhc, setSessionDynFlags,
                                           setTargets, tm_renamed_source,
                                           tm_typechecked_source,
                                           typecheckModule, unLoc)
import           GHC.Data.Graph.Directed  (topologicalSortG)
import           GHC.Driver.Env           (hsc_units)
import           GHC.Driver.Monad         (pushLogHookM)
import           GHC.Types.Error          (mkLocMessage)
import           GHC.Types.Name.Reader    (GlobalRdrEnv)
import           GHC.Types.SrcLoc         (noLoc)
import           GHC.Unit.Info            (unitId, unitPackageNameString)
import           GHC.Unit.Module.Graph    (mgModSummaries',
                                           moduleGraphNodeModSum,
                                           moduleGraphNodes, summaryNodeSummary)
import           GHC.Unit.Module.Location (ml_hs_file)
import           GHC.Unit.State           (listUnitInfo)
import           GHC.Unit.Types           (unitIdString)
import           GHC.Utils.Logger         (LogAction, getLogger,
                                           log_default_user_context)
import           GHC.Utils.Outputable     (renderWithContext)
import           System.Directory         (createDirectory,
                                           getTemporaryDirectory, removeFile,
                                           removePathForcibly)
import           System.Environment       (lookupEnv)
import           System.FilePath          ((</>))
import           System.IO                (hClose, openTempFile)
import           System.Process           (readProcess)

newtype GhcConfig
  = GhcConfig { ghcLibDir :: FilePath }
  deriving (Eq, Show)

data LoadedGhcModules
  = LoadedGhcModules
      { loadedGhcConfig        :: GhcConfig
      , loadedGhcArguments     :: [String]
      , loadedUnitPackageNames :: [(String, String)]
      , loadedModules          :: [LoadedModule]
      }

instance Show LoadedGhcModules where
  show loaded =
    "LoadedGhcModules { loadedGhcConfig = "
      ++ show (loadedGhcConfig loaded)
      ++ ", loadedGhcArguments = "
      ++ show (loadedGhcArguments loaded)
      ++ ", loadedUnitPackageNames = "
      ++ show (loadedUnitPackageNames loaded)
      ++ ", loadedModules = "
      ++ show (loadedModules loaded)
      ++ " }"

data LoadedModule
  = LoadedModule
      { loadedModuleName        :: String
      , loadedModuleFile        :: Maybe FilePath
      , loadedModuleSource      :: Maybe String
      , loadedModuleIsInternal  :: Bool
      , loadedGlobalRdrEnv      :: Maybe GlobalRdrEnv
      , loadedRenamedSource     :: Maybe RenamedSource
      , loadedTypecheckedSource :: Maybe TypecheckedSource
      }

instance Show LoadedModule where
  show loaded =
    "LoadedModule { loadedModuleName = "
      ++ show (loadedModuleName loaded)
      ++ ", loadedModuleFile = "
      ++ show (loadedModuleFile loaded)
      ++ ", loadedModuleSource = "
      ++ show (isJust (loadedModuleSource loaded))
      ++ ", loadedModuleIsInternal = "
      ++ show (loadedModuleIsInternal loaded)
      ++ ", loadedGlobalRdrEnv = "
      ++ show (isJust (loadedGlobalRdrEnv loaded))
      ++ ", loadedRenamedSource = "
      ++ show (isJust (loadedRenamedSource loaded))
      ++ ", loadedTypecheckedSource = "
      ++ show (isJust (loadedTypecheckedSource loaded))
      ++ " }"

loadExecutableModules :: PackageInfo -> ExecutableInfo -> IO (Either BundleError LoadedGhcModules)
loadExecutableModules packageInfo executableInfo = do
  libDirResult <- resolveGhcLibDir
  case libDirResult of
    Left message -> pure (Left (GhcSessionFailed message))
    Right libDir -> do
      loaded <-
        withSyntheticPathsModule packageInfo $ \syntheticSourceDirs ->
          try (runGhc (Just libDir) (loadInSession packageInfo executableInfo libDir syntheticSourceDirs))
      case loaded of
        Left err -> pure (Left (GhcLoadFailed (show (err :: SomeException))))
        Right result -> pure result

resolveGhcLibDir :: IO (Either String FilePath)
resolveGhcLibDir = do
  envValue <- lookupEnv "GHC_LIBDIR"
  case envValue of
    Just path | not (null path) -> pure (Right path)
    _ -> do
      result <- try (readProcess "ghc" ["--print-libdir"] "")
      case result of
        Left err     -> pure (Left (show (err :: SomeException)))
        Right output -> pure (Right (trim output))

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

loadInSession :: PackageInfo -> ExecutableInfo -> FilePath -> [FilePath] -> Ghc (Either BundleError LoadedGhcModules)
loadInSession packageInfo executableInfo libDir syntheticSourceDirs = do
  diagnosticsRef <- liftIO (newIORef [])
  pushLogHookM (captureDiagnostics diagnosticsRef)
  dflags0 <- getSessionDynFlags
  logger <- getLogger
  let allSourceDirs =
        nub (syntheticSourceDirs ++ executableSourceDirs executableInfo ++ packageLibrarySourceDirs packageInfo)
      arguments = ghcArguments packageInfo executableInfo allSourceDirs
  (dflags1, leftovers, _warnings) <- parseDynamicFlags logger dflags0 (map noLoc arguments)
  if not (null leftovers)
    then pure (Left (GhcSessionFailed ("Unrecognized GHC options: " ++ show (map unLocString leftovers))))
    else do
      _ <- setSessionDynFlags dflags1
      target <- guessTarget (executableMainPath executableInfo) Nothing Nothing
      setTargets [target]
      success <- load LoadAllTargets
      diagnostics <- liftIO (readIORef diagnosticsRef)
      case success of
        Failed -> pure (Left (GhcLoadFailed (loadFailureMessage executableInfo diagnostics)))
        Succeeded -> do
          moduleGraph <- getModuleGraph
          unitPackageNames <- currentUnitPackageNames
          let summaries = moduleGraphSummariesInDependencyOrder moduleGraph
          loaded <- mapM (toLoadedModule allSourceDirs) summaries
          pure
            ( Right
                LoadedGhcModules
                  { loadedGhcConfig = GhcConfig libDir
                  , loadedGhcArguments = arguments
                  , loadedUnitPackageNames = unitPackageNames
                  , loadedModules = loaded
                  }
            )

currentUnitPackageNames :: Ghc [(String, String)]
currentUnitPackageNames = do
  session <- getSession
  pure . nub $
    [ (unitIdString (unitId unitInfo), unitPackageNameString unitInfo)
    | unitInfo <- listUnitInfo (hsc_units session)
    ]

captureDiagnostics :: IORef [String] -> LogAction -> LogAction
captureDiagnostics diagnosticsRef originalLogAction flags messageClass sourceSpan message = do
  let rendered =
        renderWithContext
          (log_default_user_context flags)
          (mkLocMessage messageClass sourceSpan message)
  modifyIORef' diagnosticsRef (++ [rendered])
  originalLogAction flags messageClass sourceSpan message

loadFailureMessage :: ExecutableInfo -> [String] -> String
loadFailureMessage executableInfo diagnostics =
  unlines $
    ["Could not load executable target: " ++ executableMainPath executableInfo]
      ++ ["GHC diagnostics:" | not (null diagnostics)]
      ++ diagnostics

ghcArguments :: PackageInfo -> ExecutableInfo -> [FilePath] -> [String]
ghcArguments packageInfo executableInfo sourceDirs =
  ["-fno-code"]
    ++ sourceDirArgs
    ++ packageArgs
    ++ extensionArgs
    ++ packageLibraryCompilerOptions packageInfo
    ++ executableCompilerOptions executableInfo
  where
    sourceDirArgs =
      map ("-i" ++) sourceDirs
    packageArgs =
      concatMap
        (\packageNameValue -> ["-package", packageNameValue])
        ( filter
            (/= packageName packageInfo)
            ( nub
                ( executableDependencyPackageNames executableInfo
                    ++ packageLibraryDependencyPackageNames packageInfo
                )
            )
        )
    extensionArgs =
      map ("-X" ++) $
        nub
          ( packageLibraryDefaultExtensions packageInfo
              ++ executableDefaultExtensions executableInfo
          )

withSyntheticPathsModule :: PackageInfo -> ([FilePath] -> IO a) -> IO a
withSyntheticPathsModule packageInfo action =
  bracket
    (createSyntheticPathsModule packageInfo)
    removePathForcibly
    (\directory -> action [directory])

createSyntheticPathsModule :: PackageInfo -> IO FilePath
createSyntheticPathsModule packageInfo = do
  directory <- createTempDirectoryFromSystem "oj-hs-bundler-paths"
  writeFile
    (directory </> (packagePathsModuleName packageInfo ++ ".hs"))
    (renderSyntheticPathsModule packageInfo)
  pure directory

createTempDirectoryFromSystem :: String -> IO FilePath
createTempDirectoryFromSystem template = do
  tmp <- getTemporaryDirectory
  (path, handle) <- openTempFile tmp template
  hClose handle
  removeFile path
  createDirectory path
  pure path

renderSyntheticPathsModule :: PackageInfo -> String
renderSyntheticPathsModule packageInfo =
  unlines
    [ "module " ++ packagePathsModuleName packageInfo
    , "  ( version"
    , "  , getBinDir"
    , "  , getLibDir"
    , "  , getDynLibDir"
    , "  , getDataDir"
    , "  , getLibexecDir"
    , "  , getSysconfDir"
    , "  , getDataFileName"
    , "  ) where"
    , ""
    , "import Data.Version (Version(..))"
    , "import qualified Prelude"
    , ""
    , "version :: Version"
    , "version = Version " ++ renderVersionNumbers (packageVersionNumbers packageInfo) ++ " []"
    , ""
    , "getBinDir :: Prelude.IO Prelude.FilePath"
    , "getBinDir = Prelude.pure \".\""
    , ""
    , "getLibDir :: Prelude.IO Prelude.FilePath"
    , "getLibDir = Prelude.pure \".\""
    , ""
    , "getDynLibDir :: Prelude.IO Prelude.FilePath"
    , "getDynLibDir = Prelude.pure \".\""
    , ""
    , "getDataDir :: Prelude.IO Prelude.FilePath"
    , "getDataDir = Prelude.pure \".\""
    , ""
    , "getLibexecDir :: Prelude.IO Prelude.FilePath"
    , "getLibexecDir = Prelude.pure \".\""
    , ""
    , "getSysconfDir :: Prelude.IO Prelude.FilePath"
    , "getSysconfDir = Prelude.pure \".\""
    , ""
    , "getDataFileName :: Prelude.FilePath -> Prelude.IO Prelude.FilePath"
    , "getDataFileName name = Prelude.pure name"
    ]

renderVersionNumbers :: [Int] -> String
renderVersionNumbers [] = "[0]"
renderVersionNumbers numbers =
  "[" ++ intercalate ", " (map show numbers) ++ "]"

toLoadedModule :: [FilePath] -> ModSummary -> Ghc LoadedModule
toLoadedModule sourceDirs summary = do
  parsed <- parseModule summary
  typechecked <- typecheckModule parsed
  let info = moduleInfo typechecked
      sourceFile = ml_hs_file (ms_location summary)
  source <- liftIO (traverse readFile sourceFile)
  pure
    LoadedModule
      { loadedModuleName = moduleNameString (ms_mod_name summary)
      , loadedModuleFile = sourceFile
      , loadedModuleSource = source
      , loadedModuleIsInternal =
          case sourceFile of
            Nothing       -> False
            Just filePath -> any (`isPathPrefixOf` filePath) sourceDirs
      , loadedGlobalRdrEnv = modInfoRdrEnv info
      , loadedRenamedSource = tm_renamed_source typechecked
      , loadedTypecheckedSource = Just (tm_typechecked_source typechecked)
      }

isPathPrefixOf :: FilePath -> FilePath -> Bool
isPathPrefixOf prefix path =
  normalisePath prefix `isPrefixOfString` normalisePath path

normalisePath :: FilePath -> FilePath
normalisePath value =
  let trimmed = reverse (dropWhile (== '/') (reverse value))
   in if null trimmed
        then "/"
        else if trimmed == "."
          then ""
          else trimmed

isPrefixOfString :: String -> String -> Bool
isPrefixOfString [] _              = True
isPrefixOfString _ []              = False
isPrefixOfString (x : xs) (y : ys) = x == y && isPrefixOfString xs ys

unLocString :: Located String -> String
unLocString =
  unLoc

moduleGraphSummariesInDependencyOrder :: ModuleGraph -> [ModSummary]
moduleGraphSummariesInDependencyOrder moduleGraph =
  let (graph, _) = moduleGraphNodes False (mgModSummaries' moduleGraph)
      sortedNodes = reverse (topologicalSortG graph)
   in mapMaybe (moduleGraphNodeModSum . summaryNodeSummary) sortedNodes
