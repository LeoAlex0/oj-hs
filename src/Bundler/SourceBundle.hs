module Bundler.SourceBundle
  ( generateSourceBundle
  ) where

import Data.Char (isSpace)
import Data.List (isPrefixOf, nub, sort)
import Data.Maybe (maybeToList)
import Bundler.Cabal (ExecutableInfo (..), PackageInfo)
import Bundler.DCE (analyzeCoreLiveSet, pruneByCoreLiveSet)
import Bundler.Error (BundleError (SourceBundleFailed, SymbolConflict))
import Bundler.GHC (LoadedGhcModules (..), LoadedModule (..))
import Bundler.Rename
  ( NameTransform
  , generatedIdentifierFromName
  , transformGeneratedIdentifier
  , transformOriginalModule
  , transformOriginalOccurrence
  )
import Bundler.Transform
  ( collectRenderedExternalModules
  , collectRenamedNames
  , renderRenamedDeclarations
  )
import GHC.Types.Name (Name, nameOccName)
import GHC.Types.Name.Occurrence (NameSpace, occNameSpace, occNameString)

data SourceModule = SourceModule
  { sourceModuleName :: String
  , sourceModulePath :: FilePath
  , sourceModulePragmas :: [String]
  , sourceModuleDeclarations :: [String]
  , sourceModuleDeclarationMappings :: [DeclarationMapping]
  }
  deriving (Eq, Show)

data DeclarationMapping = DeclarationMapping
  { mappingOriginalModule :: String
  , mappingOriginalOccurrence :: String
  , mappingGeneratedIdentifier :: String
  , mappingDeclarationGroup :: String
  }
  deriving (Eq, Ord, Show)

generateSourceBundle ::
  PackageInfo ->
  ExecutableInfo ->
  LoadedGhcModules ->
  IO (Either BundleError String)
generateSourceBundle _packageInfo executableInfo loaded = do
  let internalModules = filter loadedModuleIsInternal (loadedModules loaded)
      internalNames = map loadedModuleName internalModules
      emittedModules = filter shouldEmitModule internalModules
  case findGeneratedNameConflict internalNames emittedModules of
    Just (left, right) -> pure (Left (SymbolConflict left right))
    Nothing -> do
      parsedModules <- traverse (readSourceModule internalNames) emittedModules
      case sequence parsedModules of
        Left message -> pure (Left (SourceBundleFailed message))
        Right sourceModules ->
          case findEntryBinding internalNames emittedModules of
            Left message -> pure (Left (SourceBundleFailed message))
            Right entryBinding ->
              let externalImports = "Prelude" : collectBundleExternalModules internalNames emittedModules
                  candidateSource = renderBundleSource executableInfo externalImports entryBinding sourceModules
               in do
                    liveSetResult <-
                      analyzeCoreLiveSet
                        (loadedGhcConfig loaded)
                        (loadedGhcArguments loaded)
                        candidateSource
                    case liveSetResult of
                      Left err -> pure (Left err)
                      Right liveSet ->
                        let _liveDeclarationGroups =
                              liveDeclarationGroups liveSet (concatMap sourceModuleDeclarationMappings sourceModules)
                         in pure (Right candidateSource)

shouldEmitModule :: LoadedModule -> Bool
shouldEmitModule loadedModule =
  loadedModuleName loadedModule /= "Main"

readSourceModule :: [String] -> LoadedModule -> IO (Either String SourceModule)
readSourceModule internalModuleNames loadedModule =
  case (loadedModuleFile loadedModule, loadedRenamedSource loadedModule) of
    (Nothing, _) ->
      pure (Left ("Internal module has no source file: " ++ loadedModuleName loadedModule))
    (_, Nothing) ->
      pure (Left ("Internal module has no renamed source: " ++ loadedModuleName loadedModule))
    (Just path, Just renamedSource) -> do
      source <- readFile path
      let sourceLines = lines source
          pragmas = filter isPragmaLine sourceLines
          declarations = renderRenamedDeclarations internalModuleNames (loadedGlobalRdrEnv loadedModule) renamedSource
          declarationMappings = buildDeclarationMappings internalModuleNames loadedModule
      pure
        ( Right
            SourceModule
              { sourceModuleName = loadedModuleName loadedModule
              , sourceModulePath = path
              , sourceModulePragmas = pragmas
              , sourceModuleDeclarations = trimBlankEdges declarations
              , sourceModuleDeclarationMappings = declarationMappings
              }
        )

renderBundleSource :: ExecutableInfo -> [String] -> String -> [SourceModule] -> String
renderBundleSource executableInfo externalImports entryBinding sourceModules =
  unlines $
    let retainedPragmas = uniqueSorted (concatMap sourceModulePragmas sourceModules)
     in retainedPragmas
      ++ semanticSensitivePragmaNotes retainedPragmas
      ++ [ "{-# OPTIONS_GHC -Wno-unused-imports #-}"
         , "{-# OPTIONS_GHC -Wno-unused-top-binds #-}"
         , "module Main (main) where"
         , ""
         ]
      ++ map renderQualifiedImport (uniqueSorted externalImports)
      ++ [""]
      ++ concatMap renderSourceModule sourceModules
      ++ [ "main :: Prelude.IO ()"
         , "main = " ++ entryBinding
         , ""
         , "-- bundled executable: " ++ executableName executableInfo
         ]

renderSourceModule :: SourceModule -> [String]
renderSourceModule sourceModule =
  [ "-- source: " ++ sourceModuleName sourceModule ++ " (" ++ sourceModulePath sourceModule ++ ")"
  ]
    ++ sourceModuleDeclarations sourceModule
    ++ [""]

renderQualifiedImport :: String -> String
renderQualifiedImport moduleNameValue =
  "import qualified " ++ moduleNameValue

semanticSensitivePragmaNotes :: [String] -> [String]
semanticSensitivePragmaNotes pragmas =
  [ "-- bundler note: semantic-sensitive extension retained globally: " ++ extension
  | extension <- semanticSensitiveExtensions
  , any (mentionsLanguageExtension extension) pragmas
  ]

semanticSensitiveExtensions :: [String]
semanticSensitiveExtensions =
  ["NoImplicitPrelude", "RebindableSyntax", "QualifiedDo"]

mentionsLanguageExtension :: String -> String -> Bool
mentionsLanguageExtension extension pragma =
  ("{-# LANGUAGE" `isPrefixOf` trimLeft pragma)
    && extension `elem` words (map normalizePragmaChar pragma)

normalizePragmaChar :: Char -> Char
normalizePragmaChar char
  | char `elem` "{#-}," = ' '
  | otherwise = char

collectBundleExternalModules :: [String] -> [LoadedModule] -> [String]
collectBundleExternalModules internalModuleNames loadedModules =
  uniqueSorted
    [ moduleName
    | loadedModule <- loadedModules
    , renamedSource <- maybeToList (loadedRenamedSource loadedModule)
    , moduleName <- collectRenderedExternalModules internalModuleNames (loadedGlobalRdrEnv loadedModule) renamedSource
    ]

loadedModuleNames :: LoadedModule -> [Name]
loadedModuleNames loadedModule =
  maybe [] collectRenamedNames (loadedRenamedSource loadedModule)

buildDeclarationMappings :: [String] -> LoadedModule -> [DeclarationMapping]
buildDeclarationMappings internalModuleNames loadedModule =
  sort . nub $
    [ DeclarationMapping
        { mappingOriginalModule = transformOriginalModule transform
        , mappingOriginalOccurrence = transformOriginalOccurrence transform
        , mappingGeneratedIdentifier = transformGeneratedIdentifier transform
        , mappingDeclarationGroup = loadedModuleName loadedModule
        }
    | name <- loadedModuleNames loadedModule
    , transform <- maybeToList (generatedIdentifierFromName internalModuleNames name)
    , transformOriginalModule transform == loadedModuleName loadedModule
    ]

liveDeclarationGroups :: Bundler.DCE.CoreLiveSet -> [DeclarationMapping] -> [String]
liveDeclarationGroups liveSet mappings =
  uniqueSorted
    [ mappingDeclarationGroup mapping
    | (_identifier, mapping) <-
        pruneByCoreLiveSet
          liveSet
          [(mappingGeneratedIdentifier mapping, mapping) | mapping <- mappings]
    ]

findEntryBinding :: [String] -> [LoadedModule] -> Either String String
findEntryBinding internalModuleNames loadedModules =
  case entryCandidates of
    [entryBinding] -> Right entryBinding
    [] -> Left "Could not find transformed internal main binding"
    candidates -> Left ("Multiple transformed internal main bindings: " ++ show candidates)
  where
    entryCandidates =
      sort . nub $
        [ transformGeneratedIdentifier transform
        | loadedModule <- loadedModules
        , name <- loadedModuleNames loadedModule
        , transform <- maybeToList (generatedIdentifierFromName internalModuleNames name)
        , transformOriginalOccurrence transform == "main"
        ]

findGeneratedNameConflict :: [String] -> [LoadedModule] -> Maybe (String, String)
findGeneratedNameConflict internalModuleNames loadedModules =
  findConflict generatedNames
  where
    generatedNames =
      [ (name, transform)
      | loadedModule <- loadedModules
      , name <- loadedModuleNames loadedModule
      , transform <- maybeToList (generatedIdentifierFromName internalModuleNames name)
      ]

findConflict :: [(Name, NameTransform)] -> Maybe (String, String)
findConflict [] = Nothing
findConflict ((name, transform) : rest) =
  case [ (otherName, otherTransform)
       | (otherName, otherTransform) <- rest
       , name /= otherName
       , conflictKey name transform == conflictKey otherName otherTransform
       ] of
    (otherName, otherTransform) : _ ->
      Just (describeGeneratedName name transform, describeGeneratedName otherName otherTransform)
    [] -> findConflict rest

conflictKey :: Name -> NameTransform -> (NameSpace, String)
conflictKey name transform =
  (occNameSpace (nameOccName name), transformGeneratedIdentifier transform)

describeGeneratedName :: Name -> NameTransform -> String
describeGeneratedName name transform =
  transformOriginalModule transform
    ++ "."
    ++ transformOriginalOccurrence transform
    ++ " ("
    ++ occNameString (nameOccName name)
    ++ " -> "
    ++ transformGeneratedIdentifier transform
    ++ ")"

isPragmaLine :: String -> Bool
isPragmaLine line =
  let trimmed = trimLeft line
   in "{-# LANGUAGE" `isPrefixOf` trimmed
        || "{-# OPTIONS_GHC" `isPrefixOf` trimmed

uniqueSorted :: [String] -> [String]
uniqueSorted =
  sort . nub . filter (not . null)

trimBlankEdges :: [String] -> [String]
trimBlankEdges =
  dropWhile (null . trim) . reverse . dropWhile (null . trim) . reverse

trim :: String -> String
trim =
  trimLeft . reverse . trimLeft . reverse

trimLeft :: String -> String
trimLeft =
  dropWhile isSpace
