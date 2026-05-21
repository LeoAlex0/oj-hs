module Bundler.SourceBundle
  ( generateSourceBundle
  ) where

import           Bundler.Cabal             (ExecutableInfo (..),
                                            PackageInfo (..))
import           Bundler.DCE               (CoreLiveSet (..),
                                            analyzeCoreLiveSet)
import           Bundler.Error             (BundleError (GhcLoadFailed, SourceBundleFailed, SymbolConflict))
import           Bundler.GHC               (LoadedGhcModules (..),
                                            LoadedModule (..))
import           Bundler.Rename            (NameStyle,
                                            NameTransform,
                                            generatedIdentifierFromNameWithStyle,
                                            transformGeneratedIdentifier,
                                            transformOriginalModule,
                                            transformOriginalOccurrence)
import           Bundler.Transform         (ExternalImport (..),
                                            collectExternalIdentifierRewrites,
                                            collectRenamedNames,
                                            collectRenderedExternalImports,
                                            renderRenamedDeclarations)
import           Data.Char                 (isAsciiLower, isAsciiUpper, isDigit,
                                            isSpace)
import           Data.List                 (intercalate, isInfixOf, isPrefixOf,
                                            isSuffixOf, nub, sort, stripPrefix)
import           Data.Maybe                (fromMaybe, listToMaybe,
                                            maybeToList)
import qualified Data.Set                  as Set
import           GHC.Types.Name            (Name, nameOccName)
import           GHC.Types.Name.Occurrence (NameSpace, occNameSpace,
                                            occNameString)
import           GHC.Types.Name.Reader     (GlobalRdrEnv)
import           System.FilePath           (normalise)

data SourceModule
  = SourceModule
      { sourceModuleName              :: String
      , sourceModulePragmas           :: [String]
      , sourceModuleDeclarationGroups :: [DeclarationGroup]
      }
  deriving (Eq, Show)

data DeclarationGroup
  = DeclarationGroup
      { declarationGroupId                         :: String
      , declarationGroupLines                      :: [String]
      , declarationGroupMappings                   :: [DeclarationMapping]
      , declarationGroupDefinedIdentifiers         :: [String]
      , declarationGroupReferencedIdentifiers      :: [String]
      , declarationGroupCanPrune                   :: Bool
      , declarationGroupRequiresOpaqueEitherHelper :: Bool
      }
  deriving (Eq, Show)

data DeclarationMapping
  = DeclarationMapping
      { mappingOriginalModule      :: String
      , mappingOriginalOccurrence  :: String
      , mappingGeneratedIdentifier :: String
      , mappingDeclarationGroup    :: String
      }
  deriving (Eq, Ord, Show)

generateSourceBundle ::
  NameStyle ->
  PackageInfo ->
  ExecutableInfo ->
  LoadedGhcModules ->
  IO (Either BundleError String)
generateSourceBundle nameStyle packageInfo executableInfo loaded = do
  let internalModules = filter loadedModuleIsInternal (loadedModules loaded)
      internalNames = map loadedModuleName internalModules
      internalGlobalRdrEnvs = loadedInternalGlobalRdrEnvs internalModules
      unitPackageNames = loadedUnitPackageNames loaded
      emittedModules = filter shouldEmitModule internalModules
      allDeclarationMappings = concatMap (buildDeclarationMappings nameStyle internalNames) emittedModules
  case findGeneratedNameConflict nameStyle internalNames emittedModules of
    Just (left, right) -> pure (Left (SymbolConflict left right))
    Nothing -> do
      parsedModules <- traverse (readSourceModule nameStyle internalNames internalGlobalRdrEnvs allDeclarationMappings) emittedModules
      case sequence parsedModules of
        Left message -> pure (Left (SourceBundleFailed message))
        Right sourceModules ->
          case findEntryBinding nameStyle internalNames executableInfo emittedModules of
            Left message -> pure (Left (SourceBundleFailed message))
            Right entryBinding ->
              let externalImports =
                    uniqueExternalImports
                      ( ExternalImport Nothing "Prelude"
                          : collectBundleExternalImports unitPackageNames internalNames internalGlobalRdrEnvs emittedModules
                      )
               in do
                    result <- analyzeAndRenderBundle packageInfo loaded executableInfo externalImports entryBinding sourceModules
                    case result of
                      Left err
                        | shouldRepairOpaqueEitherConstructors err ->
                            analyzeAndRenderBundle
                              packageInfo
                              loaded
                              executableInfo
                              externalImports
                              entryBinding
                              (repairOpaqueEitherConstructorsInSourceModules sourceModules)
                      _ -> pure result

analyzeAndRenderBundle ::
  PackageInfo ->
  LoadedGhcModules ->
  ExecutableInfo ->
  [ExternalImport] ->
  String ->
  [SourceModule] ->
  IO (Either BundleError String)
analyzeAndRenderBundle packageInfo loaded executableInfo externalImports entryBinding sourceModules = do
  let candidateSource = renderBundleSource packageInfo executableInfo externalImports entryBinding sourceModules
  liveSetResult <-
    analyzeCoreLiveSet
      (loadedGhcConfig loaded)
      (loadedGhcArguments loaded)
      candidateSource
  case liveSetResult of
    Left err -> pure (Left err)
    Right liveSet ->
      let prunedModules =
            compactSourceModules liveSet (pruneSourceModules liveSet entryBinding sourceModules)
          prunedSource =
            renderBundleSource packageInfo executableInfo externalImports entryBinding prunedModules
       in pure (Right prunedSource)

shouldRepairOpaqueEitherConstructors :: BundleError -> Bool
shouldRepairOpaqueEitherConstructors (GhcLoadFailed message) =
  "Illegal term-level use of the type constructor or class" `isInfixOf` message
shouldRepairOpaqueEitherConstructors _ =
  False

shouldEmitModule :: LoadedModule -> Bool
shouldEmitModule _loadedModule =
  True

loadedInternalGlobalRdrEnvs :: [LoadedModule] -> [(String, GlobalRdrEnv)]
loadedInternalGlobalRdrEnvs loadedModules =
  [ (loadedModuleName loadedModule, globalRdrEnv)
  | loadedModule <- loadedModules
  , globalRdrEnv <- maybeToList (loadedGlobalRdrEnv loadedModule)
  ]

readSourceModule :: NameStyle -> [String] -> [(String, GlobalRdrEnv)] -> [DeclarationMapping] -> LoadedModule -> IO (Either String SourceModule)
readSourceModule nameStyle internalModuleNames internalGlobalRdrEnvs allDeclarationMappings loadedModule =
  case (loadedModuleFile loadedModule, loadedModuleSource loadedModule, loadedRenamedSource loadedModule) of
    (Nothing, _, _) ->
      pure (Left ("Internal module has no source file: " ++ loadedModuleName loadedModule))
    (_, Nothing, _) ->
      pure (Left ("Internal module source was not captured: " ++ loadedModuleName loadedModule))
    (_, _, Nothing) ->
      pure (Left ("Internal module has no renamed source: " ++ loadedModuleName loadedModule))
    (_, Just source, Just renamedSource) -> do
      let sourceLines = lines source
          pragmas = filter isPragmaLine sourceLines
          renderedDeclarations =
            trimBlankEdges
              (renderRenamedDeclarations nameStyle internalModuleNames internalGlobalRdrEnvs (loadedGlobalRdrEnv loadedModule) renamedSource)
          declarationMappings =
            [ mapping
            | mapping <- allDeclarationMappings
            , mappingOriginalModule mapping == loadedModuleName loadedModule
            ]
          externalRewrites =
            collectExternalIdentifierRewrites
              internalModuleNames
              internalGlobalRdrEnvs
              (loadedGlobalRdrEnv loadedModule)
              renamedSource
          declarations =
            applyExternalIdentifierRewrites
              externalRewrites
              (applyDeclarationMappings declarationMappings renderedDeclarations)
          declarationGroups =
            buildDeclarationGroups
              (loadedModuleName loadedModule)
              allDeclarationMappings
              declarations
      pure
        ( Right
            SourceModule
              { sourceModuleName = loadedModuleName loadedModule
              , sourceModulePragmas = pragmas
              , sourceModuleDeclarationGroups = declarationGroups
              }
        )

renderBundleSource :: PackageInfo -> ExecutableInfo -> [ExternalImport] -> String -> [SourceModule] -> String
renderBundleSource packageInfo executableInfo externalImports entryBinding sourceModules =
  unlines $
    let retainedPragmas =
          normalizePragmas
            ( concatMap sourceModulePragmas sourceModules
                ++ map renderLanguagePragma (bundleDefaultExtensions packageInfo executableInfo)
                ++ ["{-# LANGUAGE PackageImports #-}" | any usesPackageImport externalImports]
                ++ [ "{-# OPTIONS_GHC -Wno-unused-imports #-}"
                   , "{-# OPTIONS_GHC -Wno-unused-top-binds #-}"
                   ]
            )
     in retainedPragmas
      ++ [ "module Main (main) where"
         ]
      ++ map renderQualifiedImport externalImports
      ++ renderOpaqueEitherHelper sourceModules
      ++ concatMap renderSourceModule sourceModules
      ++ [ "main :: Prelude.IO ()"
         , "main = " ++ entryBinding
         ]

renderSourceModule :: SourceModule -> [String]
renderSourceModule sourceModule =
  concatMap renderDeclarationGroup (sourceModuleDeclarationGroups sourceModule)

renderDeclarationGroup :: DeclarationGroup -> [String]
renderDeclarationGroup group =
  declarationGroupLines group

renderLanguagePragma :: String -> String
renderLanguagePragma extension =
  "{-# LANGUAGE " ++ extension ++ " #-}"

normalizePragmas :: [String] -> [String]
normalizePragmas =
  uniqueSorted . concatMap normalizePragma

normalizePragma :: String -> [String]
normalizePragma line
  | Just extensions <- parsePragmaBody "LANGUAGE" line =
      map renderLanguagePragma (parseLanguageExtensions extensions)
  | Just options <- parsePragmaBody "OPTIONS_GHC" line =
      [renderOptionsGhcPragma options]
  | otherwise = [trim line]

parsePragmaBody :: String -> String -> Maybe String
parsePragmaBody pragmaName line = do
  afterOpen <- stripPrefix "{-#" (trimLeft line)
  afterName <- stripPrefix pragmaName (trimLeft afterOpen)
  pure (trim (dropPragmaClose afterName))

dropPragmaClose :: String -> String
dropPragmaClose value =
  let trimmed = trim value
   in if "#-}" `isSuffixOf` trimmed
        then take (length trimmed - 3) trimmed
        else trimmed

parseLanguageExtensions :: String -> [String]
parseLanguageExtensions =
  words . map normalizeExtensionSeparator
  where
    normalizeExtensionSeparator ',' = ' '
    normalizeExtensionSeparator char = char

renderOptionsGhcPragma :: String -> String
renderOptionsGhcPragma options =
  "{-# OPTIONS_GHC " ++ unwords (words options) ++ " #-}"

bundleDefaultExtensions :: PackageInfo -> ExecutableInfo -> [String]
bundleDefaultExtensions packageInfo executableInfo =
  packageLibraryDefaultExtensions packageInfo ++ executableDefaultExtensions executableInfo

renderQualifiedImport :: ExternalImport -> String
renderQualifiedImport externalImport =
  "import qualified "
    ++ maybe "" (\packageName -> show packageName ++ " ") (externalImportPackage externalImport)
    ++ externalImportModule externalImport

usesPackageImport :: ExternalImport -> Bool
usesPackageImport externalImport =
  case externalImportPackage externalImport of
    Just _  -> True
    Nothing -> False

renderOpaqueEitherHelper :: [SourceModule] -> [String]
renderOpaqueEitherHelper sourceModules
  | any sourceModuleRequiresOpaqueEitherHelper sourceModules =
      [ opaqueEitherHelperName ++ " :: Prelude.String -> Prelude.Either Prelude.String a"
      , "{-# NOINLINE " ++ opaqueEitherHelperName ++ " #-}"
      , opaqueEitherHelperName ++ " = Prelude.Left"
      ]
  | otherwise = []

sourceModuleRequiresOpaqueEitherHelper :: SourceModule -> Bool
sourceModuleRequiresOpaqueEitherHelper sourceModule =
  any declarationGroupRequiresOpaqueEitherHelper (sourceModuleDeclarationGroups sourceModule)

opaqueEitherHelperName :: String
opaqueEitherHelperName =
  "bundler_internal_opaque_either"

compactSourceModules :: CoreLiveSet -> [SourceModule] -> [SourceModule]
compactSourceModules liveSet sourceModules =
  map compactSourceModule sourceModules
  where
    liveIdentifiers = liveGeneratedIdentifiers liveSet
    sourceReferences = sourceReferencedIdentifiers sourceModules

    compactSourceModule sourceModule =
      sourceModule
        { sourceModuleDeclarationGroups =
            map (compactDeclarationGroup liveIdentifiers sourceReferences)
              (filter (shouldRetainDeclarationGroup liveIdentifiers) (sourceModuleDeclarationGroups sourceModule))
        }

sourceReferencedIdentifiers :: [SourceModule] -> Set.Set String
sourceReferencedIdentifiers sourceModules =
  Set.fromList
    [ token
    | sourceModule <- sourceModules
    , group <- sourceModuleDeclarationGroups sourceModule
    , let groupLines = declarationGroupLines group
    , not (isClassGroup groupLines)
    , not (isInstanceGroup groupLines)
    , line <- groupLines
    , token <- generatedIdentifierTokens line
    ]

shouldRetainDeclarationGroup :: Set.Set String -> DeclarationGroup -> Bool
shouldRetainDeclarationGroup liveIdentifiers group
  | isInstanceGroup (declarationGroupLines group) =
      instanceGroupIsLive liveIdentifiers (declarationGroupLines group)
  | otherwise = True

compactDeclarationGroup :: Set.Set String -> Set.Set String -> DeclarationGroup -> DeclarationGroup
compactDeclarationGroup liveIdentifiers sourceReferences group =
  group
    { declarationGroupLines =
        compactGroupLines liveIdentifiers sourceReferences (declarationGroupLines group)
    }

compactGroupLines :: Set.Set String -> Set.Set String -> [String] -> [String]
compactGroupLines liveIdentifiers sourceReferences group
  | isDataOrNewtypeGroup group =
      rewriteDerivingBlock (derivedClassIsLive liveIdentifiers group) group
  | isClassGroup group =
      removeUnusedClassMethodLines liveIdentifiers sourceReferences group
  | isInstanceGroup group =
      removeUnusedClassMethodLines liveIdentifiers sourceReferences group
  | otherwise = group

instanceGroupIsLive :: Set.Set String -> [String] -> Bool
instanceGroupIsLive liveIdentifiers group =
  case instanceClassAndTarget group of
    Nothing -> True
    Just (className, targetName) ->
      dictionaryIsLive liveIdentifiers className targetName

instanceClassAndTarget :: [String] -> Maybe (String, String)
instanceClassAndTarget group = do
  let headText = instanceHeadText group
      classHead =
        case splitOnToken "=>" headText of
          Just (_context, instancePart) -> instancePart
          Nothing ->
            fromMaybe headText (stripPrefix "instance " headText)
      headWords = words classHead
  classWord <- listToMaybe headWords
  targetName <- listToMaybe (filter isGeneratedTypeToken (drop 1 (generatedIdentifierTokens classHead)))
  pure (lastIdentifierSegment classWord, targetName)

instanceHeadText :: [String] -> String
instanceHeadText =
  beforeWord " where" . unwords . map trim

beforeWord :: String -> String -> String
beforeWord word value =
  case splitOnToken word value of
    Just (prefix, _suffix) -> prefix
    Nothing               -> value

splitOnToken :: String -> String -> Maybe (String, String)
splitOnToken token =
  go []
  where
    go _ [] = Nothing
    go reversedPrefix remaining
      | token `isPrefixOf` remaining = Just (reverse reversedPrefix, drop (length token) remaining)
    go reversedPrefix (char : rest) =
      go (char : reversedPrefix) rest

derivedClassIsLive :: Set.Set String -> [String] -> String -> Bool
derivedClassIsLive liveIdentifiers group className =
  case declaredTypeName group of
    Nothing -> True
    Just typeName ->
      dictionaryIsLive liveIdentifiers className typeName

dictionaryIsLive :: Set.Set String -> String -> String -> Bool
dictionaryIsLive liveIdentifiers className targetName =
  any matchesDictionary (Set.toList liveIdentifiers)
  where
    classBase = lastIdentifierSegment className

    matchesDictionary identifier =
      "$f" `isPrefixOf` identifier
        && classBase `isInfixOf` identifier
        && targetName `isInfixOf` identifier

isGeneratedTypeToken :: String -> Bool
isGeneratedTypeToken (first : _) = first == 'C'
isGeneratedTypeToken []          = False

removeUnusedClassMethodLines :: Set.Set String -> Set.Set String -> [String] -> [String]
removeUnusedClassMethodLines liveIdentifiers sourceReferences group
  | isClassGroup group =
      removeMethodDefinitions liveIdentifiers sourceReferences
        (removeClassMethodSignatures liveIdentifiers sourceReferences group)
  | isInstanceGroup group = removeMethodDefinitions liveIdentifiers sourceReferences group
  | otherwise = group

removeClassMethodSignatures :: Set.Set String -> Set.Set String -> [String] -> [String]
removeClassMethodSignatures liveIdentifiers sourceReferences group =
  header ++ go body
  where
    (header, body) = splitDeclarationHeader group

    go [] = []
    go (line : rest)
      | Just methodName <- classMethodSignatureName line
      , not (methodIsLive liveIdentifiers sourceReferences methodName) =
          go (dropWhile (isMethodBlockLine (leadingSpaces line) methodName) rest)
      | otherwise = line : go rest

classMethodSignatureName :: String -> Maybe String
classMethodSignatureName line = do
  signatureLeft <- beforeToken "::" line
  let methodName = firstToken signatureLeft
  if null methodName || methodName `elem` nonMethodDeclarationTokens
    then Nothing
    else Just methodName

removeMethodDefinitions :: Set.Set String -> Set.Set String -> [String] -> [String]
removeMethodDefinitions liveIdentifiers sourceReferences group =
  header ++ go body
  where
    (header, body) = splitDeclarationHeader group

    go [] = []
    go (line : rest)
      | Just methodName <- methodDefinitionName line
      , not (methodIsLive liveIdentifiers sourceReferences methodName) =
          go (dropWhile (isMethodBlockLine (leadingSpaces line) methodName) rest)
      | otherwise = line : go rest

methodDefinitionName :: String -> Maybe String
methodDefinitionName line =
  let methodName = firstToken line
      trimmed = trimLeft line
   in if leadingSpaces line <= 0
        || null methodName
        || "{-#" `isPrefixOf` trimmed
        || methodName `elem` nonMethodDeclarationTokens
        then Nothing
        else Just methodName

nonMethodDeclarationTokens :: [String]
nonMethodDeclarationTokens =
  ["type", "data", "newtype"]

isMethodBlockLine :: Int -> String -> String -> Bool
isMethodBlockLine methodIndent methodName line =
  leadingSpaces line > methodIndent
    || (leadingSpaces line == methodIndent && firstToken line == methodName)

methodIsLive :: Set.Set String -> Set.Set String -> String -> Bool
methodIsLive liveIdentifiers sourceReferences methodName =
  Set.member methodName sourceReferences
    || Set.member methodName liveIdentifiers
    || any (methodName `isInfixOf`) (Set.toList liveIdentifiers)

splitDeclarationHeader :: [String] -> ([String], [String])
splitDeclarationHeader =
  go []
  where
    go header [] = (reverse header, [])
    go header (line : rest)
      | declarationHeaderEnds line = (reverse (line : header), rest)
      | otherwise = go (line : header) rest

declarationHeaderEnds :: String -> Bool
declarationHeaderEnds line =
  " where" `isInfixOf` line || trim line == "where"

declaredTypeName :: [String] -> Maybe String
declaredTypeName group
  | isDataOrNewtypeGroup group =
      case group of
        firstLine : _ ->
          case generatedIdentifierTokens firstLine of
            _keyword : typeName : _ -> Just typeName
            _                     -> Nothing
        [] -> Nothing
  | otherwise = Nothing

derivingBlocks :: [String] -> [[String]]
derivingBlocks =
  go
  where
    go [] = []
    go (line : rest)
      | "deriving (" `isInfixOf` line =
          let (derivingLines, remaining) = collectDerivingLines [line] rest
           in derivingLines : go remaining
      | otherwise = go rest

rewriteDerivingBlock :: (String -> Bool) -> [String] -> [String]
rewriteDerivingBlock keepClass group =
  go group
  where
    go [] = []
    go (line : rest)
      | "deriving (" `isInfixOf` line =
          let (derivingLines, remaining) = collectDerivingLines [line] rest
              classes = parseDerivingClasses derivingLines
              keptClasses = filter keepClass classes
           in renderDerivingClasses keptClasses ++ go remaining
      | otherwise = line : go rest

collectDerivingLines :: [String] -> [String] -> ([String], [String])
collectDerivingLines collected [] = (reverse collected, [])
collectDerivingLines collected rest@(line : remaining)
  | ")" `isInfixOf` head collected = (reverse collected, rest)
  | otherwise = collectDerivingLines (line : collected) remaining

parseDerivingClasses :: [String] -> [String]
parseDerivingClasses derivingLines =
  filter (not . null) (map cleanClassName rawClasses)
  where
    rawClasses =
      words
        [ if char `elem` ("()," :: String) then ' ' else char
        | char <- unwords derivingLines
        , char /= '\n'
        ]

    cleanClassName "deriving" = ""
    cleanClassName value      = trim value

renderDerivingClasses :: [String] -> [String]
renderDerivingClasses [] = []
renderDerivingClasses classes =
  ["  deriving (" ++ intercalate ", " classes ++ ")"]

isDataOrNewtypeGroup :: [String] -> Bool
isDataOrNewtypeGroup group =
  case group of
    firstLine : _ ->
      let first = trimLeft firstLine
       in "data " `isPrefixOf` first || "newtype " `isPrefixOf` first
    [] -> False

isClassGroup :: [String] -> Bool
isClassGroup group =
  case group of
    firstLine : _ -> "class " `isPrefixOf` trimLeft firstLine
    []            -> False

isInstanceGroup :: [String] -> Bool
isInstanceGroup group =
  case group of
    firstLine : _ -> "instance " `isPrefixOf` trimLeft firstLine
    []            -> False

leadingSpaces :: String -> Int
leadingSpaces =
  length . takeWhile isSpace

collectBundleExternalImports :: [(String, String)] -> [String] -> [(String, GlobalRdrEnv)] -> [LoadedModule] -> [ExternalImport]
collectBundleExternalImports unitPackageNames internalModuleNames internalGlobalRdrEnvs loadedModules =
  uniqueExternalImports
    [ externalImport
    | loadedModule <- loadedModules
    , renamedSource <- maybeToList (loadedRenamedSource loadedModule)
    , externalImport <- collectRenderedExternalImports unitPackageNames internalModuleNames internalGlobalRdrEnvs (loadedGlobalRdrEnv loadedModule) renamedSource
    ]

uniqueExternalImports :: [ExternalImport] -> [ExternalImport]
uniqueExternalImports externalImports =
  [ selectedImport moduleName
  | moduleName <- sort (nub (map externalImportModule externalImports))
  ]
  where
    selectedImport moduleName =
      case sort (nub [packageName | externalImport <- externalImports, externalImportModule externalImport == moduleName, packageName <- maybeToList (externalImportPackage externalImport)]) of
        packageName : _ -> ExternalImport (Just packageName) moduleName
        []              -> ExternalImport Nothing moduleName

loadedModuleNames :: LoadedModule -> [Name]
loadedModuleNames loadedModule =
  maybe [] collectRenamedNames (loadedRenamedSource loadedModule)

buildDeclarationMappings :: NameStyle -> [String] -> LoadedModule -> [DeclarationMapping]
buildDeclarationMappings nameStyle internalModuleNames loadedModule =
  sort . nub $
    [ DeclarationMapping
        { mappingOriginalModule = transformOriginalModule transform
        , mappingOriginalOccurrence = transformOriginalOccurrence transform
        , mappingGeneratedIdentifier = transformGeneratedIdentifier transform
        , mappingDeclarationGroup = loadedModuleName loadedModule
        }
    | name <- loadedModuleNames loadedModule
    , transform <- maybeToList (generatedIdentifierFromNameWithStyle nameStyle internalModuleNames name)
    , transformOriginalModule transform == loadedModuleName loadedModule
    ]

applyDeclarationMappings :: [DeclarationMapping] -> [String] -> [String]
applyDeclarationMappings mappings =
  map (rewriteIdentifierTokens mappings)

rewriteIdentifierTokens :: [DeclarationMapping] -> String -> String
rewriteIdentifierTokens mappings =
  rewriteHaskellLineIdentifierTokens (rewriteIdentifierToken mappings)

rewriteIdentifierToken :: [DeclarationMapping] -> String -> String
rewriteIdentifierToken mappings token =
  case [ mappingGeneratedIdentifier mapping
       | mapping <- mappings
       , identifierTokenMatches mapping token
       ] of
    replacement : _ -> replacement
    []              -> token

identifierTokenMatches :: DeclarationMapping -> String -> Bool
identifierTokenMatches mapping token =
  token == mappingGeneratedIdentifier mapping
    || lastIdentifierSegment token == mappingGeneratedIdentifier mapping

applyExternalIdentifierRewrites :: [(String, String)] -> [String] -> [String]
applyExternalIdentifierRewrites rewrites =
  go
  where
    go [] = []
    go [line] = [rewriteExternalIdentifierLine rewrites line]
    go (line : next : rest)
      | isEquationHeadLine line next =
          line : go (next : rest)
      | otherwise =
          rewriteExternalIdentifierLine rewrites line : go (next : rest)

isEquationHeadLine :: String -> String -> Bool
isEquationHeadLine line next =
  case splitExternalRewriteBoundary line of
    Just _  -> False
    Nothing -> startsWithBindingHead line && startsWithAssignmentLine next

startsWithBindingHead :: String -> Bool
startsWithBindingHead line =
  case trimLeft line of
    first : _ -> isIdentifierChar first
    []        -> False

startsWithAssignmentLine :: String -> Bool
startsWithAssignmentLine line =
  case trimLeft line of
    '=' : '=' : _ -> False
    '=' : '>' : _ -> False
    '=' : _       -> True
    _             -> False

rewriteExternalIdentifierLine :: [(String, String)] -> String -> String
rewriteExternalIdentifierLine rewrites line =
  case splitExternalRewriteBoundary line of
    Just (prefix, boundary, suffix) ->
      let rewrittenPrefix =
            if "{" `isInfixOf` prefix
              then rewriteExternalIdentifierTokens rewrites prefix
              else prefix
       in rewrittenPrefix ++ boundary ++ rewriteExternalIdentifierTokens rewrites suffix
    Nothing ->
      rewriteExternalIdentifierTokens rewrites line

splitExternalRewriteBoundary :: String -> Maybe (String, String, String)
splitExternalRewriteBoundary line =
  case firstSignatureBoundary line of
    Just boundary -> Just boundary
    Nothing       -> firstAssignmentBoundary line

firstSignatureBoundary :: String -> Maybe (String, String, String)
firstSignatureBoundary =
  go []
  where
    go _ [] = Nothing
    go reversedPrefix remaining@('"' : _) =
      let (literal, next) = consumeStringLiteral remaining
       in go (reverse literal ++ reversedPrefix) next
    go reversedPrefix remaining@('\'' : _)
      | Just (literal, next) <- consumeCharLiteral remaining =
          go (reverse literal ++ reversedPrefix) next
    go reversedPrefix remaining
      | "::" `isPrefixOf` remaining = Just (reverse reversedPrefix, "::", drop 2 remaining)
    go reversedPrefix (char : rest) =
      go (char : reversedPrefix) rest

firstAssignmentBoundary :: String -> Maybe (String, String, String)
firstAssignmentBoundary =
  go Nothing []
  where
    go _ _ [] = Nothing
    go _ reversedPrefix remaining@('"' : _) =
      let (literal, next) = consumeStringLiteral remaining
       in go (lastMaybe literal) (reverse literal ++ reversedPrefix) next
    go _ reversedPrefix remaining@('\'' : _)
      | Just (literal, next) <- consumeCharLiteral remaining =
          go (lastMaybe literal) (reverse literal ++ reversedPrefix) next
    go previous reversedPrefix ('=' : next : rest)
      | next == '=' || next == '>' =
          go (Just next) (next : '=' : reversedPrefix) rest
      | previous == Just '<' || previous == Just '>' || previous == Just '!' =
          go (Just next) (next : '=' : reversedPrefix) rest
      | otherwise =
          Just (reverse reversedPrefix, "=", next : rest)
    go previous reversedPrefix ['=']
      | previous == Just '<' || previous == Just '>' || previous == Just '!' =
          Nothing
      | otherwise =
          Just (reverse reversedPrefix, "=", "")
    go _ reversedPrefix (char : rest) =
      go (Just char) (char : reversedPrefix) rest

rewriteExternalIdentifierTokens :: [(String, String)] -> String -> String
rewriteExternalIdentifierTokens rewrites =
  rewriteHaskellLineIdentifierTokens (rewriteExternalIdentifierToken rewrites)

rewriteExternalIdentifierToken :: [(String, String)] -> String -> String
rewriteExternalIdentifierToken rewrites token
  | '.' `elem` token = token
  | otherwise = fromMaybe token (lookup token rewrites)

rewriteHaskellLineIdentifierTokens :: (String -> String) -> String -> String
rewriteHaskellLineIdentifierTokens rewrite =
  go
  where
    go [] = []
    go line@('"' : _) =
      let (literal, next) = consumeStringLiteral line
       in literal ++ go next
    go line@('\'' : _)
      | Just (literal, next) <- consumeCharLiteral line =
          literal ++ go next
    go line@(char : rest)
      | isQualifiedIdentifierChar char =
          let (tokenRest, next) = span isQualifiedIdentifierChar rest
              token = char : tokenRest
           in rewrite token ++ go next
      | otherwise = char : go rest

consumeStringLiteral :: String -> (String, String)
consumeStringLiteral [] = ([], [])
consumeStringLiteral ('"' : rest) =
  let (body, next, _closed) = consumeQuoted '"' rest
   in ('"' : body, next)
consumeStringLiteral value =
  ([], value)

consumeCharLiteral :: String -> Maybe (String, String)
consumeCharLiteral ('\'' : rest) =
  let (body, next, closed) = consumeQuoted '\'' rest
   in if closed
        then Just ('\'' : body, next)
        else Nothing
consumeCharLiteral _ =
  Nothing

consumeQuoted :: Char -> String -> (String, String, Bool)
consumeQuoted _ [] = ([], [], False)
consumeQuoted quote ('\\' : escaped : rest) =
  let (body, next, closed) = consumeQuoted quote rest
   in ('\\' : escaped : body, next, closed)
consumeQuoted _ ['\\'] = (['\\'], [], False)
consumeQuoted quote (char : rest)
  | char == quote = ([char], rest, True)
  | otherwise =
      let (body, next, closed) = consumeQuoted quote rest
       in (char : body, next, closed)

lastMaybe :: [a] -> Maybe a
lastMaybe []         = Nothing
lastMaybe [value]    = Just value
lastMaybe (_ : rest) = lastMaybe rest

repairOpaqueEitherConstructorsInSourceModules :: [SourceModule] -> [SourceModule]
repairOpaqueEitherConstructorsInSourceModules =
  map repairSourceModule
  where
    repairSourceModule sourceModule =
      let repairedGroups =
            map repairDeclarationGroup (sourceModuleDeclarationGroups sourceModule)
       in sourceModule
            { sourceModuleDeclarationGroups = repairedGroups
            }

    repairDeclarationGroup group =
      let (repairedLines, didRepair) =
            repairOpaqueEitherConstructorExpressions (declarationGroupLines group)
       in group
            { declarationGroupLines = repairedLines
            , declarationGroupRequiresOpaqueEitherHelper =
                declarationGroupRequiresOpaqueEitherHelper group || didRepair
            }

repairOpaqueEitherConstructorExpressions :: [String] -> ([String], Bool)
repairOpaqueEitherConstructorExpressions [] = ([], False)
repairOpaqueEitherConstructorExpressions [line] = ([line], False)
repairOpaqueEitherConstructorExpressions (line : next : rest)
  | rightToken (lastToken (trim line)) && "(" `isPrefixOf` trimLeft next =
      case collectEitherStringExpression (next : rest) of
        Just (bodyLines, annotationLine, remaining)
          | hasOpaqueEitherConstructor bodyLines annotationLine ->
              let (repairedRemaining, _didRepairRemaining) =
                    repairOpaqueEitherConstructorExpressions remaining
               in ( replaceRightWithLeftPayload (line : bodyLines ++ [annotationLine]) line
                      : annotationLine
                      : repairedRemaining
                  , True
                  )
        _ ->
          let (repairedRest, didRepair) =
                repairOpaqueEitherConstructorExpressions (next : rest)
           in (line : repairedRest, didRepair)
  | otherwise =
      let (repairedRest, didRepair) =
            repairOpaqueEitherConstructorExpressions (next : rest)
       in (line : repairedRest, didRepair)

hasOpaqueEitherConstructor :: [String] -> String -> Bool
hasOpaqueEitherConstructor bodyLines annotationLine =
  case firstConstructorToken bodyLines of
    Nothing -> False
    Just constructorToken ->
      "." `isInfixOf` constructorToken
        && constructorToken `isInfixOf` annotationLine
        && "Either " `isInfixOf` annotationLine

firstConstructorToken :: [String] -> Maybe String
firstConstructorToken [] = Nothing
firstConstructorToken (line : rest) =
  case trimLeft line of
    '(' : value ->
      case takeWhile isQualifiedIdentifierChar value of
        []    -> firstConstructorToken rest
        token -> Just token
    _ -> firstConstructorToken rest

replaceRightWithLeftPayload :: [String] -> String -> String
replaceRightWithLeftPayload originalLines line =
  let token = lastToken (trim line)
      replacement = opaqueEitherHelperName ++ " " ++ show (unlines originalLines) ++ " ::"
   in replaceLineSuffix token replacement line

rightToken :: String -> Bool
rightToken token =
  token == "Right" || ".Right" `isSuffixOf` token

collectEitherStringExpression :: [String] -> Maybe ([String], String, [String])
collectEitherStringExpression [] = Nothing
collectEitherStringExpression (line : rest)
  | "Either " `isInfixOf` line = Just ([], line, rest)
  | otherwise = do
      (bodyLines, annotationLine, remaining) <- collectEitherStringExpression rest
      pure (line : bodyLines, annotationLine, remaining)

replaceLineSuffix :: String -> String -> String -> String
replaceLineSuffix suffix replacement line =
  let reversedSuffix = reverse suffix
      reversedLine = reverse line
   in case stripPrefix reversedSuffix reversedLine of
        Just reversedPrefix -> reverse reversedPrefix ++ replacement
        Nothing             -> line

lastToken :: String -> String
lastToken value =
  case words value of
    []     -> ""
    tokens -> last tokens

lastIdentifierSegment :: String -> String
lastIdentifierSegment token =
  case break (== '.') token of
    (_segment, [])          -> token
    (_segment, _dot : rest) -> lastIdentifierSegment rest

buildDeclarationGroups :: String -> [DeclarationMapping] -> [String] -> [DeclarationGroup]
buildDeclarationGroups moduleNameValue allMappings declarations =
  [ buildDeclarationGroup index groupLines
  | (index, groupLines) <- zip [(1 :: Int) ..] (splitTopLevelDeclarationGroups declarations)
  ]
  where
    allKnownIdentifiers = uniqueSorted (map mappingGeneratedIdentifier allMappings)
    moduleMappings =
      [ mapping
      | mapping <- allMappings
      , mappingOriginalModule mapping == moduleNameValue
      ]
    moduleKnownIdentifiers = uniqueSorted (map mappingGeneratedIdentifier moduleMappings)

    buildDeclarationGroup index groupLines =
      let groupId = moduleNameValue ++ "#" ++ show index
          definedIdentifiers = declaredGeneratedIdentifiers moduleKnownIdentifiers groupLines
          groupMappings =
            [ mapping {mappingDeclarationGroup = groupId}
            | mapping <- moduleMappings
            , mappingGeneratedIdentifier mapping `elem` definedIdentifiers
            ]
          referencedIdentifiers = mentionedGeneratedIdentifiers allKnownIdentifiers groupLines
       in DeclarationGroup
            { declarationGroupId = groupId
            , declarationGroupLines = groupLines
            , declarationGroupMappings = groupMappings
            , declarationGroupDefinedIdentifiers = definedIdentifiers
            , declarationGroupReferencedIdentifiers = referencedIdentifiers
            , declarationGroupCanPrune =
                safeDeclarationGroup groupLines && not (null groupMappings)
            , declarationGroupRequiresOpaqueEitherHelper = False
            }

pruneSourceModules :: CoreLiveSet -> String -> [SourceModule] -> [SourceModule]
pruneSourceModules liveSet entryBinding sourceModules =
  [ sourceModule
      { sourceModuleDeclarationGroups =
          filter (shouldRetainGroup retainedGroupIds) (sourceModuleDeclarationGroups sourceModule)
      }
  | sourceModule <- sourceModules
  ]
  where
    groups = concatMap sourceModuleDeclarationGroups sourceModules
    seedIdentifiers = Set.insert entryBinding (liveGeneratedIdentifiers liveSet)
    retainedGroupIds = retainedDeclarationGroups seedIdentifiers groups

shouldRetainGroup :: Set.Set String -> DeclarationGroup -> Bool
shouldRetainGroup retainedGroupIds group =
  Set.member (declarationGroupId group) retainedGroupIds

retainedDeclarationGroups :: Set.Set String -> [DeclarationGroup] -> Set.Set String
retainedDeclarationGroups seedIdentifiers groups =
  go seedIdentifiers Set.empty
  where
    go requiredIdentifiers retainedIds =
      let retainedGroups =
            [ group
            | group <- groups
            , not (declarationGroupCanPrune group)
                || any (`Set.member` requiredIdentifiers) (declarationGroupDefinedIdentifiers group)
            ]
          nextRetainedIds = Set.fromList (map declarationGroupId retainedGroups)
          nextRequiredIdentifiers =
            Set.union
              requiredIdentifiers
              (Set.fromList (concatMap declarationGroupReferencedIdentifiers retainedGroups))
       in if nextRetainedIds == retainedIds
            then retainedIds
            else go nextRequiredIdentifiers nextRetainedIds

splitTopLevelDeclarationGroups :: [String] -> [[String]]
splitTopLevelDeclarationGroups =
  filter (not . null) . map trimBlankEdges . reverse . go [] []
  where
    go groups current [] =
      flush groups current
    go groups current (line : rest)
      | null (trim line) =
          go (flush groups current) [] rest
      | isTopLevelDeclarationLine line =
          go (flush groups current) [line] rest
      | otherwise =
          go groups (current ++ [line]) rest

    flush groups current
      | null current = groups
      | otherwise = current : groups

isTopLevelDeclarationLine :: String -> Bool
isTopLevelDeclarationLine line =
  case line of
    []        -> False
    first : _ -> not (isSpace first)

declaredGeneratedIdentifiers :: [String] -> [String] -> [String]
declaredGeneratedIdentifiers knownIdentifiers groupLines =
  uniqueSorted
    [ identifier
    | line <- groupLines
    , identifier <- declaredGeneratedIdentifiersInLine knownIdentifiers line
    ]

declaredGeneratedIdentifiersInLine :: [String] -> String -> [String]
declaredGeneratedIdentifiersInLine knownIdentifiers line
  | not (isTopLevelDeclarationLine line) = []
  | startsWithAny ["data ", "newtype ", "class ", "type "] trimmedLine =
      mentionedGeneratedIdentifiers knownIdentifiers [line]
  | startsWithAny ["infix ", "infixl ", "infixr "] trimmedLine =
      mentionedGeneratedIdentifiers knownIdentifiers [line]
  | Just signatureLeft <- beforeToken "::" line =
      mentionedGeneratedIdentifiers knownIdentifiers [signatureLeft]
  | Just bindingLeft <- beforeToken "=" line =
      let mentionedIdentifiers = mentionedGeneratedIdentifiers knownIdentifiers [bindingLeft]
          firstBindingToken = firstToken bindingLeft
          prefixBinder =
            [firstBindingToken | firstBindingToken `elem` knownIdentifiers]
          operatorBinders = filter isOperatorIdentifier mentionedIdentifiers
       in uniqueSorted (prefixBinder ++ operatorBinders)
  | otherwise = []
  where
    trimmedLine = trimLeft line

mentionedGeneratedIdentifiers :: [String] -> [String] -> [String]
mentionedGeneratedIdentifiers knownIdentifiers groupLines =
  uniqueSorted
    [ identifier
    | identifier <- knownIdentifiers
    , identifier `elem` tokens
    ]
  where
    tokens = concatMap generatedIdentifierTokens groupLines

safeDeclarationGroup :: [String] -> Bool
safeDeclarationGroup groupLines =
  case dropWhile (null . trim) groupLines of
    [] -> False
    firstLine : _ ->
      not (startsWithAny unsafePrefixes (trimLeft firstLine))
  where
    unsafePrefixes =
      [ "instance "
      , "deriving "
      , "default "
      , "foreign import "
      , "foreign export "
      , "{-#"
      , "--"
      ]

generatedIdentifierTokens :: String -> [String]
generatedIdentifierTokens [] = []
generatedIdentifierTokens (char : rest)
  | isIdentifierChar char =
      let (tokenRest, next) = span isIdentifierChar rest
       in (char : tokenRest) : generatedIdentifierTokens next
  | isOperatorChar char =
      let (tokenRest, next) = span isOperatorChar rest
       in (char : tokenRest) : generatedIdentifierTokens next
  | otherwise = generatedIdentifierTokens rest

beforeToken :: String -> String -> Maybe String
beforeToken token =
  go []
  where
    go _ [] = Nothing
    go reversedPrefix remaining@(next : rest)
      | token `isPrefixOf` remaining = Just (reverse reversedPrefix)
      | otherwise = go (next : reversedPrefix) rest

firstToken :: String -> String
firstToken value =
  case generatedIdentifierTokens value of
    token : _ -> token
    []        -> ""

startsWithAny :: [String] -> String -> Bool
startsWithAny prefixes value =
  any (`isPrefixOf` value) prefixes

isIdentifierChar :: Char -> Bool
isIdentifierChar char =
  isAlphaNumAscii char || char == '_' || char == '\''

isQualifiedIdentifierChar :: Char -> Bool
isQualifiedIdentifierChar char =
  isIdentifierChar char || char == '.'

isAlphaNumAscii :: Char -> Bool
isAlphaNumAscii char =
  isAsciiLower char || isAsciiUpper char || isDigit char

isOperatorIdentifier :: String -> Bool
isOperatorIdentifier (first : _) = isOperatorChar first
isOperatorIdentifier []          = False

isOperatorChar :: Char -> Bool
isOperatorChar char =
  char `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

findEntryBinding :: NameStyle -> [String] -> ExecutableInfo -> [LoadedModule] -> Either String String
findEntryBinding nameStyle internalModuleNames executableInfo loadedModules =
  selectEntryBinding entryCandidates
  where
    entryModules =
      case filter (isExecutableEntryModule executableInfo) loadedModules of
        []      -> filter ((== "Main") . loadedModuleName) loadedModules
        modules -> modules
    entryCandidates =
      sort . nub $
        [ ( transformOriginalModule transform == loadedModuleName loadedModule
          , transformGeneratedIdentifier transform
          )
        | loadedModule <- entryModules
        , name <- loadedModuleNames loadedModule
        , transform <- maybeToList (generatedIdentifierFromNameWithStyle nameStyle internalModuleNames name)
        , transformOriginalOccurrence transform == "main"
        ]

selectEntryBinding :: [(Bool, String)] -> Either String String
selectEntryBinding candidates =
  case localCandidates of
    [entryBinding] -> Right entryBinding
    [] ->
      case allCandidates of
        [entryBinding] -> Right entryBinding
        [] -> Left "Could not find transformed internal main binding"
        bindings -> Left ("Multiple transformed internal main bindings: " ++ show bindings)
    bindings -> Left ("Multiple transformed local main bindings: " ++ show bindings)
  where
    localCandidates = uniqueSorted [binding | (True, binding) <- candidates]
    allCandidates = uniqueSorted (map snd candidates)

isExecutableEntryModule :: ExecutableInfo -> LoadedModule -> Bool
isExecutableEntryModule executableInfo loadedModule =
  case loadedModuleFile loadedModule of
    Nothing -> False
    Just filePath -> normalise filePath == normalise (executableMainPath executableInfo)

findGeneratedNameConflict :: NameStyle -> [String] -> [LoadedModule] -> Maybe (String, String)
findGeneratedNameConflict nameStyle internalModuleNames loadedModules =
  findConflict generatedNames
  where
    generatedNames =
      [ (name, transform)
      | loadedModule <- loadedModules
      , name <- loadedModuleNames loadedModule
      , transform <- maybeToList (generatedIdentifierFromNameWithStyle nameStyle internalModuleNames name)
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
