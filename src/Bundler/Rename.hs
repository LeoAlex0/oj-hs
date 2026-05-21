module Bundler.Rename
  ( NameStyle (..)
  , NameOrigin (..)
  , NameTransform (..)
  , classifyName
  , collectExternalModules
  , detectNameTransformConflict
  , generatedIdentifier
  , generatedIdentifierWithStyle
  , generatedIdentifierFromName
  , generatedIdentifierFromNameWithStyle
  ) where

import           Data.Bits                           (xor)
import           Data.Char                           (isAlphaNum, isUpper, ord)
import           Data.List                           (nub, sort)
import           Data.Word                           (Word64)
import           GHC.Types.Name                      (Name, isWiredInName,
                                                      nameModule_maybe,
                                                      nameOccName)
import           GHC.Types.Name.Occurrence           (OccName, isSymOcc,
                                                      isVarNameSpace,
                                                      occNameSpace,
                                                      occNameString)
import           GHC.Unit.Types                      (moduleName)
import           Language.Haskell.Syntax.Module.Name (moduleNameString)

data NameOrigin
  = InternalName String
  | ExternalName String
  | LocalName
  | WiredInName
  deriving (Eq, Show)

data NameStyle = ReadableNames | CompactNames
  deriving (Eq, Show)

data NameTransform
  = NameTransform
      { transformOriginalModule      :: String
      , transformOriginalOccurrence  :: String
      , transformGeneratedIdentifier :: String
      }
  deriving (Eq, Show)

data GeneratedNameCategory = VarIdentifier | ConstructorIdentifier | VariableOperator | ConstructorOperator

classifyName :: [String] -> Name -> NameOrigin
classifyName internalModules name
  | isWiredInName name = WiredInName
  | otherwise =
      case nameModule_maybe name of
        Nothing -> LocalName
        Just nameModuleValue ->
          let moduleNameValue = moduleNameString (moduleName nameModuleValue)
           in if moduleNameValue `elem` internalModules
                then InternalName moduleNameValue
                else ExternalName moduleNameValue

collectExternalModules :: [String] -> [Name] -> [String]
collectExternalModules internalModules names =
  sort . nub $
    [ moduleNameValue
    | ExternalName moduleNameValue <- map (classifyName internalModules) names
    ]

detectNameTransformConflict :: [NameTransform] -> Maybe (NameTransform, NameTransform)
detectNameTransformConflict [] = Nothing
detectNameTransformConflict (transform : rest) =
  case filter (isGeneratedNameConflict transform) rest of
    conflict : _ -> Just (transform, conflict)
    []           -> detectNameTransformConflict rest

isGeneratedNameConflict :: NameTransform -> NameTransform -> Bool
isGeneratedNameConflict left right =
  transformGeneratedIdentifier left == transformGeneratedIdentifier right
    && ( transformOriginalModule left /= transformOriginalModule right
          || transformOriginalOccurrence left /= transformOriginalOccurrence right
       )

generatedIdentifier :: String -> String -> NameTransform
generatedIdentifier = generatedIdentifierWithStyle ReadableNames

generatedIdentifierWithStyle :: NameStyle -> String -> String -> NameTransform
generatedIdentifierWithStyle nameStyle sourceModuleName occurrenceName =
  NameTransform
    { transformOriginalModule = sourceModuleName
    , transformOriginalOccurrence = occurrenceName
    , transformGeneratedIdentifier =
        generatedName nameStyle sourceModuleName occurrenceName (categoryFromSpelling occurrenceName)
    }

generatedIdentifierFromName :: [String] -> Name -> Maybe NameTransform
generatedIdentifierFromName = generatedIdentifierFromNameWithStyle ReadableNames

generatedIdentifierFromNameWithStyle :: NameStyle -> [String] -> Name -> Maybe NameTransform
generatedIdentifierFromNameWithStyle nameStyle internalModules name =
  case classifyName internalModules name of
    InternalName moduleNameValue ->
      let originalOccName = nameOccName name
       in Just
            NameTransform
              { transformOriginalModule = moduleNameValue
              , transformOriginalOccurrence = occNameString originalOccName
              , transformGeneratedIdentifier =
                  generatedName
                    nameStyle
                    moduleNameValue
                    (occNameString originalOccName)
                    (categoryFromOccName originalOccName)
              }
    _ -> Nothing

generatedName :: NameStyle -> String -> String -> GeneratedNameCategory -> String
generatedName ReadableNames sourceModuleName occurrenceName category =
  case category of
    VarIdentifier ->
      "v_" ++ readableBase
    ConstructorIdentifier ->
      "C_" ++ readableBase
    VariableOperator ->
      "!" ++ occurrenceName ++ "!" ++ operatorBase
    ConstructorOperator ->
      ":!" ++ occurrenceName ++ "!" ++ operatorBase
  where
    readableBase =
      encodeIdentifierPart sourceModuleName ++ "_" ++ encodeIdentifierPart occurrenceName
    stableInput =
      sourceModuleName ++ "\0" ++ occurrenceName
    operatorBase =
      encodeSymbolNumber (compactHashString stableInput)
generatedName CompactNames sourceModuleName occurrenceName category =
  case category of
    VarIdentifier ->
      "v" ++ digest
    ConstructorIdentifier ->
      "C" ++ digest
    VariableOperator ->
      "!" ++ operatorBase
    ConstructorOperator ->
      ":!" ++ operatorBase
  where
    stableInput =
      sourceModuleName ++ "\0" ++ occurrenceName
    digest =
      encodeIdentifierNumber (compactHashString stableInput)
    operatorBase =
      encodeSymbolNumber (compactHashString stableInput)

encodeIdentifierPart :: String -> String
encodeIdentifierPart =
  concatMap encodeIdentifierChar

encodeIdentifierChar :: Char -> String
encodeIdentifierChar char
  | isAlphaNum char = [char]
  | otherwise = "_" ++ show (ord char) ++ "_"

categoryFromOccName :: OccName -> GeneratedNameCategory
categoryFromOccName occNameValue
  | isSymOcc occNameValue =
      categoryFromOperatorSpelling (occNameString occNameValue)
  | isVarNameSpace (occNameSpace occNameValue) = VarIdentifier
  | otherwise = ConstructorIdentifier

categoryFromSpelling :: String -> GeneratedNameCategory
categoryFromSpelling value@(first : _)
  | isOperatorChar first = categoryFromOperatorSpelling value
  | isUpper first = ConstructorIdentifier
  | otherwise = VarIdentifier
categoryFromSpelling [] = VarIdentifier

categoryFromOperatorSpelling :: String -> GeneratedNameCategory
categoryFromOperatorSpelling (':' : _) = ConstructorOperator
categoryFromOperatorSpelling _         = VariableOperator

encodeIdentifierNumber :: Word64 -> String
encodeIdentifierNumber 0 = [identifierDigit 0]
encodeIdentifierNumber value =
  reverse (go value)
  where
    base = fromIntegral (length identifierDigits)

    go 0 = []
    go current =
      let (next, digit) = current `quotRem` base
       in identifierDigit (fromIntegral digit) : go next

identifierDigit :: Int -> Char
identifierDigit value =
  identifierDigits !! value

identifierDigits :: String
identifierDigits =
  ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

stableHashString :: String -> Word64
stableHashString =
  foldl hashStep 14695981039346656037
  where
    hashStep current char =
      (current `xor` fromIntegral (ord char)) * 1099511628211

compactHashString :: String -> Word64
compactHashString value =
  stableHashString value `rem` compactHashSpace

compactHashSpace :: Word64
compactHashSpace =
  281474976710656

encodeSymbolNumber :: Word64 -> String
encodeSymbolNumber 0 = [operatorDigit 0]
encodeSymbolNumber value =
  reverse (go value)
  where
    base = fromIntegral (length operatorDigits)

    go 0 = []
    go current =
      let (next, digit) = current `quotRem` base
       in operatorDigit (fromIntegral digit) : go next

operatorDigit :: Int -> Char
operatorDigit value =
  operatorDigits !! value

operatorDigits :: String
operatorDigits =
  "!$%&*+/<>?@^|~"

isOperatorChar :: Char -> Bool
isOperatorChar char =
  char `elem` operatorChars

operatorChars :: String
operatorChars =
  "!#$%&*+./<=>?@\\^|-~:"
