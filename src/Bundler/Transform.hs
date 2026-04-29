{-# LANGUAGE RankNTypes #-}

module Bundler.Transform
  ( collectRenderedExternalModules
  , collectRenamedNames
  , renderRenamedDeclarations
  , rewriteRenamedSource
  ) where

import Data.Data (Data, cast, gmapQ, gmapT)
import Data.Char (isAlphaNum, isSpace, isUpper)
import Data.List (isPrefixOf, nub, sort, stripPrefix)
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
import GHC.Types.Name.Occurrence (OccName, mkOccName, occNameSpace)
import GHC.Types.Name.Reader
  ( GlobalRdrEnv
  , greDefinitionModule
  , gre_imp
  , is_decl
  , is_mod
  , lookupGlobalRdrEnv
  )
import GHC.Unit.Types (Module, moduleName)
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

rewriteRenamedSource :: [String] -> RenamedSource -> RenamedSource
rewriteRenamedSource internalModules =
  rewriteData (rewriteName internalModules)

renderRenamedDeclarations :: [String] -> Maybe GlobalRdrEnv -> RenamedSource -> [String]
renderRenamedDeclarations internalModules globalRdrEnv renamedSource =
  let (group, _imports, _exports, _docs) = rewriteRenamedSource internalModules renamedSource
   in repairQualifiedBinderLines (lines (renderBundleSDoc internalModules globalRdrEnv (ppr group)))

collectRenamedNames :: RenamedSource -> [Name]
collectRenamedNames =
  collectNames

collectRenderedExternalModules :: [String] -> Maybe GlobalRdrEnv -> RenamedSource -> [String]
collectRenderedExternalModules internalModules globalRdrEnv renamedSource =
  sort . nub $
    [ moduleNameString qualifierModule
    | name <- collectRenamedNames renamedSource
    , qualifierModule <- maybeToList (qualifierModuleForName internalModules globalRdrEnv name)
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

renderBundleSDoc :: [String] -> Maybe GlobalRdrEnv -> SDoc -> String
renderBundleSDoc internalModules globalRdrEnv =
  showSDocUnsafe . withUserStyle (bundleNamePprCtx internalModules globalRdrEnv) AllTheWay

bundleNamePprCtx :: [String] -> Maybe GlobalRdrEnv -> NamePprCtx
bundleNamePprCtx internalModules globalRdrEnv =
  QueryQualify
    { queryQualifyName = \nameModuleValue occNameValue ->
        maybe NameUnqual NameQual (qualifierModuleForModule internalModules globalRdrEnv nameModuleValue occNameValue)
    , queryQualifyModule = alwaysQualifyModules
    , queryQualifyPackage = neverQualifyPackages
    , queryPromotionTick = queryPromotionTick neverQualify
    }

qualifierModuleForName :: [String] -> Maybe GlobalRdrEnv -> Name -> Maybe ModuleName
qualifierModuleForName internalModules globalRdrEnv name =
  case classifyName internalModules name of
    InternalName _ -> Nothing
    LocalName -> Nothing
    WiredInName -> Nothing
    ExternalName _ ->
      case nameModule_maybe name of
        Nothing -> Nothing
        Just nameModuleValue ->
          qualifierModuleForModule internalModules globalRdrEnv nameModuleValue (nameOccName name)

qualifierModuleForModule ::
  [String] ->
  Maybe GlobalRdrEnv ->
  Module ->
  OccName ->
  Maybe ModuleName
qualifierModuleForModule internalModules globalRdrEnv nameModuleValue occNameValue
  | moduleNameString (moduleName nameModuleValue) `elem` internalModules = Nothing
  | otherwise =
      case globalRdrEnv >>= importedModuleForName nameModuleValue occNameValue of
        Just importedModule -> Just importedModule
        Nothing -> Nothing

importedModuleForName ::
  Module ->
  OccName ->
  GlobalRdrEnv ->
  Maybe ModuleName
importedModuleForName nameModuleValue occNameValue globalRdrEnv =
  case sort (nub (map moduleNameString candidateModules)) of
    [] -> Nothing
    firstModuleName : _ ->
      Just (head [candidate | candidate <- candidateModules, moduleNameString candidate == firstModuleName])
  where
    candidateModules =
      [ is_mod (is_decl importSpec)
      | gre <- lookupGlobalRdrEnv globalRdrEnv occNameValue
      , greDefinitionModule gre == Just nameModuleValue
      , importSpec <- bagToList (gre_imp gre)
      ]

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
