module Bundler.DCE
  ( CoreLiveSet (..)
  , analyzeCoreLiveSet
  , pruneByCoreLiveSet
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Bundler.Error (BundleError (GhcLoadFailed, GhcSessionFailed))
import Bundler.GHC (GhcConfig (..))
import GHC
  ( Ghc
  , LoadHowMuch (LoadAllTargets)
  , ModSummary
  , SuccessFlag (Failed, Succeeded)
  , coreModule
  , desugarModule
  , getModuleGraph
  , getSessionDynFlags
  , load
  , moduleNameString
  , ms_mod_name
  , parseDynamicFlags
  , parseModule
  , runGhc
  , setSessionDynFlags
  , setTargets
  , typecheckModule
  , unLoc
  )
import GHC.Core (CoreProgram, flattenBinds)
import GHC.Core.FVs (exprFreeIdsList)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Data.Graph.Directed (topologicalSortG)
import GHC.Driver.Main (hscSimplify)
import GHC.Driver.Monad (getSession, pushLogHookM)
import GHC.Driver.Session (homeUnitId_)
import GHC.Types.Error (mkLocMessage)
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc (noLoc)
import GHC.Types.Target
  ( Target (..)
  , TargetId (TargetFile)
  )
import GHC.Types.Var (Var, varName)
import GHC.Unit.Module.Graph
  ( ModuleGraph
  , mgModSummaries'
  , moduleGraphNodeModSum
  , moduleGraphNodes
  , summaryNodeSummary
  )
import GHC.Unit.Module.ModGuts (mg_binds)
import Data.Time.Clock (getCurrentTime)
import GHC.Utils.Logger (LogAction, getLogger, log_default_user_context)
import GHC.Utils.Outputable (renderWithContext)

newtype CoreLiveSet = CoreLiveSet
  { liveGeneratedIdentifiers :: Set.Set String
  }
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
    Left err -> pure (Left (GhcLoadFailed (show (err :: SomeException))))
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
              hscEnv <- getSession
              simplified <- liftIO (hscSimplify hscEnv [] (coreModule desugared))
              pure (Right (coreLiveSetFromBinds (mg_binds simplified)))

captureDiagnostics :: IORef [String] -> LogAction -> LogAction
captureDiagnostics diagnosticsRef originalLogAction flags messageClass sourceSpan message = do
  let rendered =
        renderWithContext
          (log_default_user_context flags)
          (mkLocMessage messageClass sourceSpan message)
  modifyIORef' diagnosticsRef (++ [rendered])
  originalLogAction flags messageClass sourceSpan message

coreLoadFailureMessage :: [String] -> String
coreLoadFailureMessage diagnostics =
  unlines $
    ["Could not load bundled candidate module"]
      ++ ["GHC diagnostics:" | not (null diagnostics)]
      ++ diagnostics

candidateGhcArguments :: [String] -> [String]
candidateGhcArguments = id

candidateSummary :: [ModSummary] -> Maybe ModSummary
candidateSummary summaries =
  case filter ((== "Main") . moduleNameString . ms_mod_name) summaries of
    summary : _ -> Just summary
    [] -> Nothing

moduleGraphSummaries :: ModuleGraph -> [ModSummary]
moduleGraphSummaries moduleGraph =
  let (graph, _) = moduleGraphNodes False (mgModSummaries' moduleGraph)
      sortedNodes = reverse (topologicalSortG graph)
   in mapMaybe (moduleGraphNodeModSum . summaryNodeSummary) sortedNodes

coreLiveSetFromBinds :: CoreProgram -> CoreLiveSet
coreLiveSetFromBinds binds =
  CoreLiveSet
    { liveGeneratedIdentifiers =
        reachableIdentifiers dependencyMap seedIdentifiers
    }
  where
    flattenedBindings = flattenBinds binds
    bindingIdentifiers =
      Map.fromList
        [ (identifierFromVar binder, expression)
        | (binder, expression) <- flattenedBindings
        ]
    localIdentifiers = Map.keysSet bindingIdentifiers
    dependencyMap =
      Map.map
        ( Set.fromList
            . filter (`Set.member` localIdentifiers)
            . map identifierFromVar
            . exprFreeIdsList
        )
        bindingIdentifiers
    seedIdentifiers =
      if Map.member "main" dependencyMap
        then Set.singleton "main"
        else localIdentifiers

identifierFromVar :: Var -> String
identifierFromVar binder =
  occNameString (nameOccName (varName binder))

reachableIdentifiers :: Map.Map String (Set.Set String) -> Set.Set String -> Set.Set String
reachableIdentifiers dependencyMap =
  go Set.empty . Set.toList
  where
    go reached [] = reached
    go reached (identifier : pending)
      | Set.member identifier reached = go reached pending
      | otherwise =
          let dependencies =
                Set.toList (Map.findWithDefault Set.empty identifier dependencyMap)
           in go (Set.insert identifier reached) (dependencies ++ pending)

pruneByCoreLiveSet :: CoreLiveSet -> [(String, a)] -> [(String, a)]
pruneByCoreLiveSet liveSet =
  filter (\(identifier, _) -> Set.member identifier (liveGeneratedIdentifiers liveSet))
