module Bundler.DCE
  ( CoreLiveSet (..)
  , analyzeCoreLiveSet
  , pruneByCoreLiveSet
  ) where

import           Bundler.Error             (BundleError (GhcLoadFailed, GhcSessionFailed))
import           Bundler.GHC               (GhcConfig (..))
import           Control.Exception         (SomeException, try)
import           Control.Monad.IO.Class    (liftIO)
import           Data.IORef                (IORef, modifyIORef', newIORef,
                                            readIORef)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (mapMaybe)
import qualified Data.Set                  as Set
import           Data.Time.Clock           (getCurrentTime)
import           GHC                       (Ghc, LoadHowMuch (LoadAllTargets),
                                            ModSummary,
                                            SuccessFlag (Failed, Succeeded),
                                            coreModule, desugarModule,
                                            getModuleGraph, getSessionDynFlags,
                                            load, moduleNameString, ms_mod_name,
                                            parseDynamicFlags, parseModule,
                                            runGhc, setSessionDynFlags,
                                            setTargets, typecheckModule, unLoc)
import           GHC.Core                  (CoreProgram, flattenBinds)
import           GHC.Core.FVs              (exprFreeIdsList)
import           GHC.Data.Graph.Directed   (topologicalSortG)
import           GHC.Data.StringBuffer     (stringToStringBuffer)
import           GHC.Driver.Monad          (pushLogHookM)
import           GHC.Driver.Session        (homeUnitId_)
import           GHC.Types.Error           (mkLocMessage)
import           GHC.Types.Name            (Name, nameOccName)
import           GHC.Types.Name.Occurrence (occNameString)
import           GHC.Types.SrcLoc          (noLoc)
import           GHC.Types.Target          (Target (..), TargetId (TargetFile))
import           GHC.Types.Var             (varName)
import           GHC.Unit.Module.Graph     (ModuleGraph, mgModSummaries',
                                            moduleGraphNodeModSum,
                                            moduleGraphNodes,
                                            summaryNodeSummary)
import           GHC.Unit.Module.ModGuts   (mg_binds)
import           GHC.Utils.Logger          (LogAction, getLogger,
                                            log_default_user_context)
import           GHC.Utils.Outputable      (renderWithContext)

newtype CoreLiveSet
  = CoreLiveSet { liveGeneratedIdentifiers :: Set.Set String }
  deriving (Eq, Show)

analyzeCoreLiveSet :: GhcConfig -> [String] -> String -> IO (Either BundleError CoreLiveSet)
analyzeCoreLiveSet ghcConfig ghcArguments candidateSource = do
  result <-
    try
      ( runGhc
          (Just (ghcLibDir ghcConfig))
          (analyzeCoreLiveSetInSession ghcArguments candidateSource)
      )
  case result of
    Left err    -> pure (Left (GhcLoadFailed (show (err :: SomeException))))
    Right value -> pure value

analyzeCoreLiveSetInSession :: [String] -> String -> Ghc (Either BundleError CoreLiveSet)
analyzeCoreLiveSetInSession ghcArguments candidateSource = do
  diagnosticsRef <- liftIO (newIORef [])
  pushLogHookM (captureDiagnostics diagnosticsRef)
  dflags0 <- getSessionDynFlags
  logger <- getLogger
  (dflags1, leftovers, _warnings) <-
    parseDynamicFlags logger dflags0 (map noLoc (candidateGhcArguments ghcArguments))
  if not (null leftovers)
    then pure (Left (GhcSessionFailed ("Unrecognized GHC options: " ++ show (map unLoc leftovers))))
    else do
      _ <- setSessionDynFlags dflags1
      timestamp <- liftIO getCurrentTime
      setTargets
        [ Target
            { targetId = TargetFile "BundledCandidate.hs" Nothing
            , targetAllowObjCode = False
            , targetUnitId = homeUnitId_ dflags1
            , targetContents = Just (stringToStringBuffer candidateSource, timestamp)
            }
        ]
      success <- load LoadAllTargets
      diagnostics <- liftIO (readIORef diagnosticsRef)
      case success of
        Failed -> pure (Left (GhcLoadFailed (coreLoadFailureMessage diagnostics)))
        Succeeded -> do
          moduleGraph <- getModuleGraph
          case candidateSummary (moduleGraphSummaries moduleGraph) of
            Nothing -> pure (Left (GhcLoadFailed "Bundled candidate Main module was not found in ModuleGraph"))
            Just summary -> do
              parsed <- parseModule summary
              typechecked <- typecheckModule parsed
              desugared <- desugarModule typechecked
              pure (Right (coreLiveSetFromBinds (mg_binds (coreModule desugared))))

captureDiagnostics :: IORef [String] -> LogAction -> LogAction
captureDiagnostics diagnosticsRef _originalLogAction flags messageClass sourceSpan message = do
  let rendered =
        renderWithContext
          (log_default_user_context flags)
          (mkLocMessage messageClass sourceSpan message)
  modifyIORef' diagnosticsRef (++ [rendered])

coreLoadFailureMessage :: [String] -> String
coreLoadFailureMessage diagnostics =
  unlines $
    ["Could not load bundled candidate module"]
      ++ ["GHC diagnostics:" | not (null diagnostics)]
      ++ diagnostics

candidateGhcArguments :: [String] -> [String]
candidateGhcArguments =
  filter (not . isOptimizationFlag)

isOptimizationFlag :: String -> Bool
isOptimizationFlag "-O" = True
isOptimizationFlag ('-' : 'O' : rest) =
  all (`elem` ("0123456789" :: String)) rest
isOptimizationFlag _ = False

candidateSummary :: [ModSummary] -> Maybe ModSummary
candidateSummary summaries =
  case filter ((== "Main") . moduleNameString . ms_mod_name) summaries of
    summary : _ -> Just summary
    []          -> Nothing

moduleGraphSummaries :: ModuleGraph -> [ModSummary]
moduleGraphSummaries moduleGraph =
  let (graph, _) = moduleGraphNodes False (mgModSummaries' moduleGraph)
      sortedNodes = reverse (topologicalSortG graph)
   in mapMaybe (moduleGraphNodeModSum . summaryNodeSummary) sortedNodes

coreLiveSetFromBinds :: CoreProgram -> CoreLiveSet
coreLiveSetFromBinds binds =
  CoreLiveSet
    { liveGeneratedIdentifiers =
        Set.map identifierFromName (reachableNames dependencyMap seedNames)
    }
  where
    flattenedBindings = flattenBinds binds
    dependencyMap =
      Map.fromListWith
        Set.union
        [ ( varName binder
          , Set.fromList (map varName (exprFreeIdsList expression))
          )
        | (binder, expression) <- flattenedBindings
        ]
    mainNames =
      Set.filter ((== "main") . identifierFromName) (Map.keysSet dependencyMap)
    seedNames =
      if Set.null mainNames
        then Map.keysSet dependencyMap
        else mainNames

identifierFromName :: Name -> String
identifierFromName name =
  occNameString (nameOccName name)

reachableNames :: Map.Map Name (Set.Set Name) -> Set.Set Name -> Set.Set Name
reachableNames dependencyMap =
  go Set.empty . Set.toList
  where
    go reached [] = reached
    go reached (name : pending)
      | Set.member name reached = go reached pending
      | otherwise =
          let dependencies =
                Set.toList (Map.findWithDefault Set.empty name dependencyMap)
           in go (Set.insert name reached) (dependencies ++ pending)

pruneByCoreLiveSet :: CoreLiveSet -> [(String, a)] -> [(String, a)]
pruneByCoreLiveSet liveSet =
  filter (\(identifier, _) -> Set.member identifier (liveGeneratedIdentifiers liveSet))
