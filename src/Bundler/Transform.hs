{-# LANGUAGE RankNTypes #-}

module Bundler.Transform
  ( ExternalImport (..)
  , collectRenderedExternalImports
  , collectExternalIdentifierRewrites
  , collectRenamedNames
  , renderRenamedDeclarations
  , rewriteRenamedSource
  ) where

import Data.Data (Data, cast, gmapQ, gmapT)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import Data.List (isInfixOf, isPrefixOf, nub, sort, stripPrefix)
import Data.Maybe (fromMaybe, maybeToList)
import Bundler.Rename
  ( NameOrigin (ExternalName, InternalName, LocalName, WiredInName)
  , classifyName
  , generatedIdentifierFromName
  , transformGeneratedIdentifier
  )
import GHC.Data.Bag (bagToList)
import GHC (RenamedSource)
import GHC.Types.Name (Name, nameModule_maybe, nameOccName, tidyNameOcc)
import GHC.Types.Name.Occurrence (OccName, mkOccName, occNameSpace, occNameString)
import GHC.Types.Name.Reader
  ( GlobalRdrEnv
  , greDefinitionModule
  , gre_imp
  , is_decl
  , is_mod
  , lookupGlobalRdrEnv
  )
import GHC.Unit.Types (Module, moduleName, moduleUnitId, unitIdString)
import GHC.Utils.Outputable
  ( Depth (AllTheWay)
  , NamePprCtx (QueryQualify)
  , QualifyName (NameQual, NameUnqual)
  , SDoc
  , alwaysQualifyModules
  , neverQualify
  , neverQualifyPackages
  , ppr
  , queryQualifyModule
  , queryQualifyName
  , queryQualifyPackage
  , queryPromotionTick
  , showSDocUnsafe
  , withUserStyle
  )
import Language.Haskell.Syntax.Module.Name (ModuleName, moduleNameString)

data ExternalImport = ExternalImport
  { externalImportPackage :: Maybe String
  , externalImportModule :: String
  }
  deriving (Eq, Ord, Show)

rewriteRenamedSource :: [String] -> RenamedSource -> RenamedSource
rewriteRenamedSource internalModules =
  rewriteData (rewriteName internalModules)

renderRenamedDeclarations :: [String] -> [(String, GlobalRdrEnv)] -> Maybe GlobalRdrEnv -> RenamedSource -> [String]
renderRenamedDeclarations internalModules internalGlobalRdrEnvs globalRdrEnv renamedSource =
  let (group, _imports, _exports, _docs) = rewriteRenamedSource internalModules renamedSource
   in repairQualifiedRecordFields
        ( repairMultilineCaseLines
            (repairQualifiedBinderLines (lines (renderBundleSDoc internalModules internalGlobalRdrEnvs globalRdrEnv (ppr group))))
        )

collectRenamedNames :: RenamedSource -> [Name]
collectRenamedNames =
  collectNames

collectRenderedExternalImports :: [String] -> [(String, GlobalRdrEnv)] -> Maybe GlobalRdrEnv -> RenamedSource -> [ExternalImport]
collectRenderedExternalImports internalModules internalGlobalRdrEnvs globalRdrEnv renamedSource =
  sort . nub $
    [ ExternalImport
        { externalImportPackage = externalImportPackageForQualifier nameModuleValue qualifierModule
        , externalImportModule = moduleNameString qualifierModule
        }
    | name <- collectRenamedNames renamedSource
    , nameModuleValue <- maybeToList (nameModule_maybe name)
    , qualifierModule <- maybeToList (qualifierModuleForName internalModules internalGlobalRdrEnvs globalRdrEnv name)
    ]

externalImportPackageForQualifier :: Module -> ModuleName -> Maybe String
externalImportPackageForQualifier nameModuleValue qualifierModule
  | moduleNameString qualifierModule == moduleNameString (moduleName nameModuleValue) =
      packageNameFromUnitId (unitIdString (moduleUnitId nameModuleValue))
  | otherwise = Nothing

packageNameFromUnitId :: String -> Maybe String
packageNameFromUnitId unitIdValue =
  case candidatePackageNames unitIdValue of
    packageName : _
      | validPackageImportName packageName -> Just packageName
    []
      | validPackageImportName unitIdValue && unitIdValue `notElem` nonPackageUnitIds ->
          Just unitIdValue
    _ -> Nothing

nonPackageUnitIds :: [String]
nonPackageUnitIds =
  ["main", "interactive"]

candidatePackageNames :: String -> [String]
candidatePackageNames unitIdValue =
  reverse
    [ prefix
    | (prefix, suffix) <- splitBeforeHyphens unitIdValue
    , startsWithVersion suffix
    ]

splitBeforeHyphens :: String -> [(String, String)]
splitBeforeHyphens value =
  go [] value
  where
    go _ [] = []
    go reversedPrefix ('-' : suffix) =
      (reverse reversedPrefix, suffix) : go ('-' : reversedPrefix) suffix
    go reversedPrefix (char : rest) =
      go (char : reversedPrefix) rest

startsWithVersion :: String -> Bool
startsWithVersion value =
  case span (\char -> isDigit char || char == '.') value of
    (versionPrefix, _) ->
      any isDigit versionPrefix && '.' `elem` versionPrefix

validPackageImportName :: String -> Bool
validPackageImportName [] = False
validPackageImportName value =
  all validPackageImportChar value

validPackageImportChar :: Char -> Bool
validPackageImportChar char =
  isAlphaNum char || char == '-'

collectExternalIdentifierRewrites :: [String] -> [(String, GlobalRdrEnv)] -> Maybe GlobalRdrEnv -> RenamedSource -> [(String, String)]
collectExternalIdentifierRewrites internalModules internalGlobalRdrEnvs globalRdrEnv renamedSource =
  let names = collectRenamedNames renamedSource
      localOccurrences =
        sort . nub $
          [ occNameString (nameOccName name)
          | name <- names
          , classifyName internalModules name == LocalName
          ]
   in unambiguousRewrites
        [ (occurrence, moduleNameString qualifierModule ++ "." ++ occurrence)
        | name <- names
        , qualifierModule <- maybeToList (qualifierModuleForName internalModules internalGlobalRdrEnvs globalRdrEnv name)
        , let occurrence = occNameString (nameOccName name)
        , isIdentifierOccurrence occurrence
        , occurrence `notElem` localOccurrences
        ]

rewriteName :: [String] -> Name -> Name
rewriteName internalModules name =
  case generatedIdentifierFromName internalModules name of
    Nothing -> name
    Just transform ->
      let originalOccName = nameOccName name
          generatedOccName =
            mkOccName
              (occNameSpace originalOccName)
              (transformGeneratedIdentifier transform)
       in tidyNameOcc name generatedOccName

rewriteData :: Data a => (Name -> Name) -> a -> a
rewriteData rewrite =
  everywhere (rewriteNameCast rewrite)

rewriteNameCast :: forall a. Data a => (Name -> Name) -> a -> a
rewriteNameCast rewrite value =
  fromMaybe value $ do
    name <- cast value
    cast (rewrite name)

everywhere :: Data a => (forall b. Data b => b -> b) -> a -> a
everywhere rewrite value =
  rewrite (gmapT (everywhere rewrite) value)

collectNames :: Data a => a -> [Name]
collectNames value =
  case cast value of
    Just name -> [name]
    Nothing -> concat (gmapQ collectNames value)

unambiguousRewrites :: [(String, String)] -> [(String, String)]
unambiguousRewrites rewrites =
  [ (occurrence, head targets)
  | occurrence <- sort (nub (map fst rewrites))
  , let targets = sort (nub [target | (candidate, target) <- rewrites, candidate == occurrence])
  , length targets == 1
  ]

isIdentifierOccurrence :: String -> Bool
isIdentifierOccurrence [] = False
isIdentifierOccurrence (first : rest) =
  (isAlpha first || first == '_')
    && all isIdentifierOccurrenceChar rest

isIdentifierOccurrenceChar :: Char -> Bool
isIdentifierOccurrenceChar char =
  isAlphaNum char || char == '_' || char == '\''

renderBundleSDoc :: [String] -> [(String, GlobalRdrEnv)] -> Maybe GlobalRdrEnv -> SDoc -> String
renderBundleSDoc internalModules internalGlobalRdrEnvs globalRdrEnv =
  showSDocUnsafe . withUserStyle (bundleNamePprCtx internalModules internalGlobalRdrEnvs globalRdrEnv) AllTheWay

bundleNamePprCtx :: [String] -> [(String, GlobalRdrEnv)] -> Maybe GlobalRdrEnv -> NamePprCtx
bundleNamePprCtx internalModules internalGlobalRdrEnvs globalRdrEnv =
  QueryQualify
    { queryQualifyName = \nameModuleValue occNameValue ->
        maybe NameUnqual NameQual (qualifierModuleForModule internalModules internalGlobalRdrEnvs globalRdrEnv nameModuleValue occNameValue)
    , queryQualifyModule = alwaysQualifyModules
    , queryQualifyPackage = neverQualifyPackages
    , queryPromotionTick = queryPromotionTick neverQualify
    }

qualifierModuleForName :: [String] -> [(String, GlobalRdrEnv)] -> Maybe GlobalRdrEnv -> Name -> Maybe ModuleName
qualifierModuleForName internalModules internalGlobalRdrEnvs globalRdrEnv name =
  case classifyName internalModules name of
    InternalName _ -> Nothing
    LocalName -> Nothing
    WiredInName -> Nothing
    ExternalName _ ->
      case nameModule_maybe name of
        Nothing -> Nothing
        Just nameModuleValue ->
          qualifierModuleForModule internalModules internalGlobalRdrEnvs globalRdrEnv nameModuleValue (nameOccName name)

qualifierModuleForModule ::
  [String] ->
  [(String, GlobalRdrEnv)] ->
  Maybe GlobalRdrEnv ->
  Module ->
  OccName ->
  Maybe ModuleName
qualifierModuleForModule internalModules internalGlobalRdrEnvs globalRdrEnv nameModuleValue occNameValue
  | moduleNameString definingModule `elem` internalModules = Nothing
  | otherwise =
      case resolveImportedQualifier internalModules internalGlobalRdrEnvs nameModuleValue occNameValue [] globalRdrEnv of
        Just importedModule -> Just importedModule
        Nothing -> Just definingModule
  where
    definingModule = moduleName nameModuleValue

resolveImportedQualifier ::
  [String] ->
  [(String, GlobalRdrEnv)] ->
  Module ->
  OccName ->
  [String] ->
  Maybe GlobalRdrEnv ->
  Maybe ModuleName
resolveImportedQualifier _ _ _ _ _ Nothing = Nothing
resolveImportedQualifier internalModules internalGlobalRdrEnvs nameModuleValue occNameValue seen (Just globalRdrEnv) =
  case firstExternalModule candidateModules of
    Just externalModule -> Just externalModule
    Nothing ->
      firstJust
        [ resolveImportedQualifier
            internalModules
            internalGlobalRdrEnvs
            nameModuleValue
            occNameValue
            (moduleNameString internalModule : seen)
            (lookup (moduleNameString internalModule) internalGlobalRdrEnvs)
        | internalModule <- candidateModules
        , let internalModuleName = moduleNameString internalModule
        , internalModuleName `elem` internalModules
        , internalModuleName `notElem` seen
        ]
  where
    candidateModules = importedModulesForName nameModuleValue occNameValue globalRdrEnv

    firstExternalModule modules =
      case sort (nub [moduleNameString candidate | candidate <- modules, moduleNameString candidate `notElem` internalModules]) of
        [] -> Nothing
        firstModuleName : _ ->
          Just (head [candidate | candidate <- modules, moduleNameString candidate == firstModuleName])

importedModulesForName ::
  Module ->
  OccName ->
  GlobalRdrEnv ->
  [ModuleName]
importedModulesForName nameModuleValue occNameValue globalRdrEnv =
  case sort (nub (map moduleNameString candidateModules)) of
    [] -> []
    moduleNames ->
      [ head [candidate | candidate <- candidateModules, moduleNameString candidate == moduleNameValue]
      | moduleNameValue <- moduleNames
      ]
  where
    candidateModules =
      [ is_mod (is_decl importSpec)
      | gre <- lookupGlobalRdrEnv globalRdrEnv occNameValue
      , greDefinitionModule gre == Just nameModuleValue
      , importSpec <- bagToList (gre_imp gre)
      ]

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just value : _) = Just value
firstJust (Nothing : rest) = firstJust rest

repairQualifiedBinderLines :: [String] -> [String]
repairQualifiedBinderLines =
  go False
  where
    go _ [] = []
    go inInstance (line : rest)
      | "instance " `isPrefixOf` line =
          line : go True rest
      | isTopLevelLine line =
          line : go False rest
      | inInstance =
          repairQualifiedBinderLine line : go True rest
      | otherwise =
          line : go False rest

repairMultilineCaseLines :: [String] -> [String]
repairMultilineCaseLines [] = []
repairMultilineCaseLines (line : rest)
  | trimLeft line == "case" =
      case break ((== "of") . trimLeft) rest of
        (scrutineeLines@(_ : _), ofLine : remaining) ->
          wrapCaseScrutinee line scrutineeLines ofLine ++ repairMultilineCaseLines remaining
        _ -> line : repairMultilineCaseLines rest
  | otherwise = line : repairMultilineCaseLines rest

wrapCaseScrutinee :: String -> [String] -> String -> [String]
wrapCaseScrutinee caseLine [] ofLine =
  [caseLine, ofLine]
wrapCaseScrutinee caseLine (firstScrutinee : restScrutineeLines) _ofLine =
  case reverse restScrutineeLines of
    [] ->
      [caseLine ++ " (" ++ trimLeft firstScrutinee ++ ") of"]
    lastScrutinee : reversedMiddle ->
      (caseLine ++ " (" ++ trimLeft firstScrutinee)
        : reverse reversedMiddle
          ++ [lastScrutinee ++ ") of"]

trimLeft :: String -> String
trimLeft =
  dropWhile isSpace

repairQualifiedRecordFields :: [String] -> [String]
repairQualifiedRecordFields =
  go Nothing Nothing
  where
    go _ _ [] = []
    go pendingQualifier activeQualifier (line : rest) =
      case activeQualifier of
        Just qualifier ->
          let repairedLine =
                if isRecordFieldLine line || hasRecordFieldAssignment line
                  then qualifyRecordFieldLine qualifier line
                  else line
              nextActiveQualifier =
                if "}" `isInfixOf` line then Nothing else Just qualifier
           in repairedLine : go Nothing nextActiveQualifier rest
        Nothing ->
          case recordStartQualifier pendingQualifier line of
            Just qualifier ->
              let repairedLine = qualifyRecordFieldLine qualifier line
                  nextActiveQualifier =
                    if "}" `isInfixOf` line then Nothing else Just qualifier
               in repairedLine : go Nothing nextActiveQualifier rest
            Nothing ->
              line : go (qualifiedConstructorModule line) Nothing rest

recordStartQualifier :: Maybe String -> String -> Maybe String
recordStartQualifier qualifier line
  | isRecordFieldLine line = qualifier
  | otherwise = Nothing

qualifyRecordFieldLine :: String -> String -> String
qualifyRecordFieldLine qualifier line =
  leadingSpaces ++ repairFieldPrefix rest
  where
    (leadingSpaces, rest) = span isSpace line

    repairFieldPrefix ('{' : afterBrace) =
      "{" ++ qualifyFieldPrefix afterBrace
    repairFieldPrefix (',' : afterComma) =
      "," ++ qualifyFieldPrefix afterComma
    repairFieldPrefix value =
      qualifyFieldPrefix value

    qualifyFieldPrefix value =
      let (spaces, fieldAndRest) = span isSpace value
          (fieldName, afterField) = span isRecordFieldChar fieldAndRest
       in if shouldQualifyField fieldName afterField
            then spaces ++ qualifier ++ "." ++ fieldName ++ afterField
            else value

shouldQualifyField :: String -> String -> Bool
shouldQualifyField fieldName afterField =
  not (null fieldName)
    && not ('.' `elem` fieldName)
    && "=" `isPrefixOf` trimLeft afterField

isRecordFieldLine :: String -> Bool
isRecordFieldLine line =
  case trimLeft line of
    '{' : rest -> hasRecordFieldAssignment rest
    ',' : rest -> hasRecordFieldAssignment rest
    _ -> False

hasRecordFieldAssignment :: String -> Bool
hasRecordFieldAssignment value =
  let trimmed = trimLeft value
      (fieldName, afterField) = span isRecordFieldChar trimmed
   in not (null fieldName) && startsWithFieldAssignment afterField

startsWithFieldAssignment :: String -> Bool
startsWithFieldAssignment value =
  case trimLeft value of
    '=' : '=' : _ -> False
    '=' : _ -> True
    _ -> False

qualifiedConstructorModule :: String -> Maybe String
qualifiedConstructorModule line =
  case [qualifier | token <- lexicalTokens line, qualifier <- maybeToList (constructorQualifier token)] of
    [] -> Nothing
    qualifiers -> Just (last qualifiers)

constructorQualifier :: String -> Maybe String
constructorQualifier token =
  let segments = splitOnDot token
   in case reverse segments of
        constructorSegment : qualifierSegments@(_ : _)
          | startsWithUpper constructorSegment
          , all startsWithUpper qualifierSegments ->
              Just (joinWithDot (reverse qualifierSegments))
        _ -> Nothing

lexicalTokens :: String -> [String]
lexicalTokens [] = []
lexicalTokens (char : rest)
  | isQualifiedTokenChar char =
      let (tokenRest, next) = span isQualifiedTokenChar rest
       in (char : tokenRest) : lexicalTokens next
  | otherwise = lexicalTokens rest

splitOnDot :: String -> [String]
splitOnDot value =
  case break (== '.') value of
    (segment, []) -> [segment]
    (segment, _dot : rest) -> segment : splitOnDot rest

joinWithDot :: [String] -> String
joinWithDot [] = ""
joinWithDot [value] = value
joinWithDot (value : rest) = value ++ "." ++ joinWithDot rest

startsWithUpper :: String -> Bool
startsWithUpper (first : _) = isUpper first
startsWithUpper [] = False

isQualifiedTokenChar :: Char -> Bool
isQualifiedTokenChar char =
  isAlphaNum char || char == '_' || char == '\'' || char == '.'

isRecordFieldChar :: Char -> Bool
isRecordFieldChar char =
  isAlphaNum char || char == '_' || char == '\''

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just value <|> _ = Just value
Nothing <|> other = other

isTopLevelLine :: String -> Bool
isTopLevelLine [] = False
isTopLevelLine (first : _) = not (isSpace first)

repairQualifiedBinderLine :: String -> String
repairQualifiedBinderLine line
  | Just rest <- stripPrefix "  type " line =
      "  type " ++ repairFirstBinderToken rest
  | Just rest <- stripPrefix "  " line
  , not (" " `isPrefixOf` rest) =
      "  " ++ repairFirstBinderToken (repairSecondBinderToken rest)
  | otherwise = line

repairFirstBinderToken :: String -> String
repairFirstBinderToken value =
  let (token, rest) = break isSpace value
   in stripQualifiedToken token ++ rest

repairSecondBinderToken :: String -> String
repairSecondBinderToken value =
  let (firstToken, restAfterFirst) = break isSpace value
      (spaces, rest) = span isSpace restAfterFirst
      (secondToken, restAfterSecond) = break isSpace rest
   in firstToken ++ spaces ++ stripQualifiedToken secondToken ++ restAfterSecond

stripQualifiedToken :: String -> String
stripQualifiedToken ('(' : rest)
  | not (null rest)
  , last rest == ')' =
      "(" ++ stripQualifiedToken (init rest) ++ ")"
stripQualifiedToken token =
  fromMaybe token (stripModuleQualifier token)

stripModuleQualifier :: String -> Maybe String
stripModuleQualifier token =
  case stripModuleSegments token of
    (True, rest) | not (null rest) -> Just rest
    _ -> Nothing

stripModuleSegments :: String -> (Bool, String)
stripModuleSegments value =
  case stripOneModuleSegment value of
    Nothing -> (False, value)
    Just rest -> go True rest
  where
    go seen rest =
      case stripOneModuleSegment rest of
        Nothing -> (seen, rest)
        Just nextRest -> go True nextRest

stripOneModuleSegment :: String -> Maybe String
stripOneModuleSegment (first : rest)
  | isUpper first =
      let (_segmentRest, afterSegment) = span isModuleSegmentChar rest
       in case afterSegment of
            '.' : afterDot -> Just afterDot
            _ -> Nothing
stripOneModuleSegment _ = Nothing

isModuleSegmentChar :: Char -> Bool
isModuleSegmentChar char =
  isAlphaNum char || char == '_' || char == '\''
