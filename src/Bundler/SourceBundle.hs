module Bundler.SourceBundle
  ( generateSourceBundle
  ) where

import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, sort, stripPrefix)
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import Bundler.Cabal (ExecutableInfo (..), PackageInfo)
import Bundler.DCE (CoreLiveSet (..), analyzeCoreLiveSet)
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
  , collectExternalIdentifierRewrites
  , collectRenamedNames
  , renderRenamedDeclarations
  )
import GHC.Types.Name (Name, nameOccName)
import GHC.Types.Name.Occurrence (NameSpace, occNameSpace, occNameString)

data SourceModule = SourceModule
  { sourceModuleName :: String
  , sourceModulePragmas :: [String]
  , sourceModuleDeclarationGroups :: [DeclarationGroup]
  }
  deriving (Eq, Show)

data DeclarationGroup = DeclarationGroup
  { declarationGroupId :: String
  , declarationGroupLines :: [String]
  , declarationGroupMappings :: [DeclarationMapping]
  , declarationGroupDefinedIdentifiers :: [String]
  , declarationGroupReferencedIdentifiers :: [String]
  , declarationGroupCanPrune :: Bool
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
                        let prunedModules = pruneSourceModules liveSet entryBinding sourceModules
                            prunedSource = renderBundleSource executableInfo externalImports entryBinding prunedModules
                         in pure (Right prunedSource)

shouldEmitModule :: LoadedModule -> Bool
shouldEmitModule _loadedModule =
  True

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
          renderedDeclarations =
            trimBlankEdges
              (renderRenamedDeclarations internalModuleNames (loadedGlobalRdrEnv loadedModule) renamedSource)
          declarationMappings = buildDeclarationMappings internalModuleNames loadedModule
          externalRewrites =
            collectExternalIdentifierRewrites
              internalModuleNames
              (loadedGlobalRdrEnv loadedModule)
              renamedSource
          declarations =
            sanitizePathsModulePathLiterals (loadedModuleName loadedModule) $
              sanitizeGitHashConstructorExpressions
                ( applyExternalIdentifierRewrites
                    externalRewrites
                    (applyDeclarationMappings declarationMappings renderedDeclarations)
                )
          declarationGroups =
            buildDeclarationGroups
              (loadedModuleName loadedModule)
              declarationMappings
              declarations
      pure
        ( Right
            SourceModule
              { sourceModuleName = loadedModuleName loadedModule
              , sourceModulePragmas = pragmas
              , sourceModuleDeclarationGroups = declarationGroups
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
  [ "-- source: " ++ sourceModuleName sourceModule
  ]
    ++ concatMap renderDeclarationGroup (sourceModuleDeclarationGroups sourceModule)
    ++ [""]

renderDeclarationGroup :: DeclarationGroup -> [String]
renderDeclarationGroup group =
  declarationGroupLines group ++ [""]

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

applyDeclarationMappings :: [DeclarationMapping] -> [String] -> [String]
applyDeclarationMappings mappings =
  map (rewriteIdentifierTokens mappings)

rewriteIdentifierTokens :: [DeclarationMapping] -> String -> String
rewriteIdentifierTokens mappings [] = []
rewriteIdentifierTokens mappings line@(char : rest)
  | isQualifiedIdentifierChar char =
      let (tokenRest, next) = span isQualifiedIdentifierChar rest
          token = char : tokenRest
       in rewriteIdentifierToken mappings token ++ rewriteIdentifierTokens mappings next
  | otherwise = char : rewriteIdentifierTokens mappings rest

rewriteIdentifierToken :: [DeclarationMapping] -> String -> String
rewriteIdentifierToken mappings token =
  case [ mappingGeneratedIdentifier mapping
       | mapping <- mappings
       , identifierTokenMatches mapping token
       ] of
    replacement : _ -> replacement
    [] -> token

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
    Just _ -> False
    Nothing -> startsWithBindingHead line && startsWithAssignmentLine next

startsWithBindingHead :: String -> Bool
startsWithBindingHead line =
  case trimLeft line of
    first : _ -> isIdentifierChar first
    [] -> False

startsWithAssignmentLine :: String -> Bool
startsWithAssignmentLine line =
  case trimLeft line of
    '=' : '=' : _ -> False
    '=' : '>' : _ -> False
    '=' : _ -> True
    _ -> False

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
    Nothing -> firstAssignmentBoundary line

firstSignatureBoundary :: String -> Maybe (String, String, String)
firstSignatureBoundary line =
  go [] line
  where
    go _ [] = Nothing
    go reversedPrefix remaining
      | "::" `isPrefixOf` remaining = Just (reverse reversedPrefix, "::", drop 2 remaining)
    go reversedPrefix (char : rest) =
      go (char : reversedPrefix) rest

firstAssignmentBoundary :: String -> Maybe (String, String, String)
firstAssignmentBoundary line =
  go Nothing [] line
  where
    go _ _ [] = Nothing
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
rewriteExternalIdentifierTokens _ [] = []
rewriteExternalIdentifierTokens rewrites (char : rest)
  | isQualifiedIdentifierChar char =
      let (tokenRest, next) = span isQualifiedIdentifierChar rest
          token = char : tokenRest
       in rewriteExternalIdentifierToken rewrites token ++ rewriteExternalIdentifierTokens rewrites next
  | otherwise = char : rewriteExternalIdentifierTokens rewrites rest

rewriteExternalIdentifierToken :: [(String, String)] -> String -> String
rewriteExternalIdentifierToken rewrites token
  | '.' `elem` token = token
  | otherwise =
      case lookup token rewrites of
        Just replacement -> replacement
        Nothing -> token

sanitizeGitHashConstructorExpressions :: [String] -> [String]
sanitizeGitHashConstructorExpressions [] = []
sanitizeGitHashConstructorExpressions [line] = [line]
sanitizeGitHashConstructorExpressions (line : next : rest)
  | isGitHashRightStart line next =
      let replacementLine = replaceGitHashRightStart line
       in replacementLine : dropGitHashConstructor rest
  | otherwise = line : sanitizeGitHashConstructorExpressions (next : rest)

isGitHashRightStart :: String -> String -> Bool
isGitHashRightStart line next =
  rightToken (lastToken (trim line))
    && "(GitHash.GitInfo" `isPrefixOf` trimLeft next

rightToken :: String -> Bool
rightToken token =
  token == "Right" || ".Right" `isSuffixOf` token

replaceGitHashRightStart :: String -> String
replaceGitHashRightStart line =
  replaceLineSuffix "Right" "Left \"\" ::" line

dropGitHashConstructor :: [String] -> [String]
dropGitHashConstructor [] = []
dropGitHashConstructor (line : rest)
  | "Either " `isInfixOf` line && "GitHash.GitInfo" `isInfixOf` line =
      line : sanitizeGitHashConstructorExpressions rest
  | otherwise =
      dropGitHashConstructor rest

replaceLineSuffix :: String -> String -> String -> String
replaceLineSuffix suffix replacement line =
  let reversedSuffix = reverse suffix
      reversedLine = reverse line
   in case stripPrefix reversedSuffix reversedLine of
        Just reversedPrefix -> reverse reversedPrefix ++ replacement
        Nothing -> line

lastToken :: String -> String
lastToken value =
  case words value of
    [] -> ""
    tokens -> last tokens

sanitizePathsModulePathLiterals :: String -> [String] -> [String]
sanitizePathsModulePathLiterals moduleNameValue declarations
  | "Paths_" `isPrefixOf` moduleNameValue =
      map sanitizeAbsolutePathAssignment declarations
  | otherwise = declarations

sanitizeAbsolutePathAssignment :: String -> String
sanitizeAbsolutePathAssignment line =
  case break (== '"') line of
    (prefix, '"' : '/' : _rest)
      | "=" `isInfixOf` prefix -> prefix ++ "\".\""
    _ -> line

lastIdentifierSegment :: String -> String
lastIdentifierSegment token =
  case break (== '.') token of
    (_segment, []) -> token
    (_segment, _dot : rest) -> lastIdentifierSegment rest

buildDeclarationGroups :: String -> [DeclarationMapping] -> [String] -> [DeclarationGroup]
buildDeclarationGroups moduleNameValue mappings declarations =
  [ buildDeclarationGroup index groupLines
  | (index, groupLines) <- zip [(1 :: Int) ..] (splitTopLevelDeclarationGroups declarations)
  ]
  where
    knownIdentifiers = uniqueSorted (map mappingGeneratedIdentifier mappings)

    buildDeclarationGroup index groupLines =
      let groupId = moduleNameValue ++ "#" ++ show index
          definedIdentifiers = declaredGeneratedIdentifiers knownIdentifiers groupLines
          groupMappings =
            [ mapping {mappingDeclarationGroup = groupId}
            | mapping <- mappings
            , mappingGeneratedIdentifier mapping `elem` definedIdentifiers
            ]
          referencedIdentifiers = mentionedGeneratedIdentifiers knownIdentifiers groupLines
       in DeclarationGroup
            { declarationGroupId = groupId
            , declarationGroupLines = groupLines
            , declarationGroupMappings = groupMappings
            , declarationGroupDefinedIdentifiers = definedIdentifiers
            , declarationGroupReferencedIdentifiers = referencedIdentifiers
            , declarationGroupCanPrune =
                safeDeclarationGroup groupLines && not (null groupMappings)
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
    [] -> False
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
beforeToken token value =
  go [] value
  where
    go _ [] = Nothing
    go reversedPrefix remaining@(next : rest)
      | token `isPrefixOf` remaining = Just (reverse reversedPrefix)
      | otherwise = go (next : reversedPrefix) rest

firstToken :: String -> String
firstToken value =
  case generatedIdentifierTokens value of
    token : _ -> token
    [] -> ""

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
  ('a' <= char && char <= 'z')
    || ('A' <= char && char <= 'Z')
    || ('0' <= char && char <= '9')

isOperatorIdentifier :: String -> Bool
isOperatorIdentifier (first : _) = isOperatorChar first
isOperatorIdentifier [] = False

isOperatorChar :: Char -> Bool
isOperatorChar char =
  char `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

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
