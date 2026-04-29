module Bundler.GHC
  ( GhcConfig (..)
  , LoadedGhcModules (..)
  , LoadedModule (..)
  , loadExecutableModules
  , resolveGhcLibDir
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Bundler.Cabal (ExecutableInfo (..), PackageInfo (..))
import Bundler.Error (BundleError (..))
import GHC.Driver.Monad (pushLogHookM)
import GHC.Types.Error (mkLocMessage)
import GHC.Data.Graph.Directed (topologicalSortG)
import GHC
  ( Ghc
  , Located
  , LoadHowMuch (LoadAllTargets)
  , ModuleGraph
  , ModSummary
  , RenamedSource
  , SuccessFlag (Failed, Succeeded)
  , TypecheckedSource
  , getModuleGraph
  , getSessionDynFlags
  , guessTarget
  , load
  , moduleNameString
  , modInfoRdrEnv
  , moduleInfo
  , ms_location
  , ms_mod_name
  , parseModule
  , parseDynamicFlags
  , runGhc
  , setSessionDynFlags
  , setTargets
  , tm_renamed_source
  , tm_typechecked_source
  , typecheckModule
  , unLoc
  )
import GHC.Types.SrcLoc (noLoc)
import GHC.Types.Name.Reader (GlobalRdrEnv)
import GHC.Unit.Module.Graph
  ( mgModSummaries'
  , moduleGraphNodeModSum
  , moduleGraphNodes
  , summaryNodeSummary
  )
import GHC.Unit.Module.Location (ml_hs_file)
import GHC.Utils.Logger (LogAction, getLogger, log_default_user_context)
import GHC.Utils.Outputable (renderWithContext)
import System.Environment (lookupEnv)
import System.Process (readProcess)

newtype GhcConfig = GhcConfig
  { ghcLibDir :: FilePath
  }
  deriving (Eq, Show)

data LoadedGhcModules = LoadedGhcModules
  { loadedGhcConfig :: GhcConfig
  , loadedGhcArguments :: [String]
  , loadedModules :: [LoadedModule]
  }

instance Show LoadedGhcModules where
  show loaded =
    "LoadedGhcModules { loadedGhcConfig = "
      ++ show (loadedGhcConfig loaded)
      ++ ", loadedGhcArguments = "
      ++ show (loadedGhcArguments loaded)
      ++ ", loadedModules = "
      ++ show (loadedModules loaded)
      ++ " }"

data LoadedModule = LoadedModule
  { loadedModuleName :: String
  , loadedModuleFile :: Maybe FilePath
  , loadedModuleIsInternal :: Bool
  , loadedGlobalRdrEnv :: Maybe GlobalRdrEnv
  , loadedRenamedSource :: Maybe RenamedSource
  , loadedTypecheckedSource :: Maybe TypecheckedSource
  }

instance Show LoadedModule where
  show loaded =
    "LoadedModule { loadedModuleName = "
      ++ show (loadedModuleName loaded)
      ++ ", loadedModuleFile = "
      ++ show (loadedModuleFile loaded)
      ++ ", loadedModuleIsInternal = "
      ++ show (loadedModuleIsInternal loaded)
      ++ ", loadedGlobalRdrEnv = "
      ++ show (maybe False (const True) (loadedGlobalRdrEnv loaded))
      ++ ", loadedRenamedSource = "
      ++ show (maybe False (const True) (loadedRenamedSource loaded))
      ++ ", loadedTypecheckedSource = "
      ++ show (maybe False (const True) (loadedTypecheckedSource loaded))
      ++ " }"

loadExecutableModules :: PackageInfo -> ExecutableInfo -> IO (Either BundleError LoadedGhcModules)
loadExecutableModules packageInfo executableInfo = do
  libDirResult <- resolveGhcLibDir
  case libDirResult of
    Left message -> pure (Left (GhcSessionFailed message))
    Right libDir -> do
      loaded <- try (runGhc (Just libDir) (loadInSession packageInfo executableInfo libDir))
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
        Left err -> pure (Left (show (err :: SomeException)))
        Right output -> pure (Right (trim output))

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

loadInSession :: PackageInfo -> ExecutableInfo -> FilePath -> Ghc (Either BundleError LoadedGhcModules)
loadInSession packageInfo executableInfo libDir = do
  diagnosticsRef <- liftIO (newIORef [])
  pushLogHookM (captureDiagnostics diagnosticsRef)
  dflags0 <- getSessionDynFlags
  logger <- getLogger
  let arguments = ghcArguments packageInfo executableInfo
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
          let allSourceDirs =
                nub (executableSourceDirs executableInfo ++ packageLibrarySourceDirs packageInfo)
              summaries = moduleGraphSummariesInDependencyOrder moduleGraph
          loaded <- mapM (toLoadedModule allSourceDirs) summaries
          pure
            ( Right
                LoadedGhcModules
                  { loadedGhcConfig = GhcConfig libDir
                  , loadedGhcArguments = arguments
                  , loadedModules = loaded
                  }
            )

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

ghcArguments :: PackageInfo -> ExecutableInfo -> [String]
ghcArguments packageInfo executableInfo =
  ["-fno-code"]
    ++ sourceDirArgs
    ++ packageArgs
    ++ extensionArgs
    ++ executableCompilerOptions executableInfo
  where
    sourceDirArgs =
      map ("-i" ++) (nub (executableSourceDirs executableInfo ++ packageLibrarySourceDirs packageInfo))
    packageArgs =
      concatMap
        (\packageNameValue -> ["-package", packageNameValue])
        (filter (/= packageName packageInfo) (nub (executableDependencyPackageNames executableInfo)))
    extensionArgs =
      map ("-X" ++) (executableDefaultExtensions executableInfo)

toLoadedModule :: [FilePath] -> ModSummary -> Ghc LoadedModule
toLoadedModule sourceDirs summary = do
  parsed <- parseModule summary
  typechecked <- typecheckModule parsed
  let info = moduleInfo typechecked
  pure
    LoadedModule
      { loadedModuleName = moduleNameString (ms_mod_name summary)
      , loadedModuleFile = ml_hs_file (ms_location summary)
      , loadedModuleIsInternal =
          case ml_hs_file (ms_location summary) of
            Nothing -> False
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
   in if null trimmed then "/" else trimmed

isPrefixOfString :: String -> String -> Bool
isPrefixOfString [] _ = True
isPrefixOfString _ [] = False
isPrefixOfString (x : xs) (y : ys) = x == y && isPrefixOfString xs ys

unLocString :: Located String -> String
unLocString located =
  unLoc located

moduleGraphSummariesInDependencyOrder :: ModuleGraph -> [ModSummary]
moduleGraphSummariesInDependencyOrder moduleGraph =
  let (graph, _) = moduleGraphNodes False (mgModSummaries' moduleGraph)
      sortedNodes = reverse (topologicalSortG graph)
   in mapMaybe (moduleGraphNodeModSum . summaryNodeSummary) sortedNodes
