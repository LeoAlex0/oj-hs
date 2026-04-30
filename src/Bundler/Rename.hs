module Bundler.Rename
  ( NameOrigin (..)
  , NameTransform (..)
  , classifyName
  , collectExternalModules
  , detectNameTransformConflict
  , generatedIdentifier
  , generatedIdentifierFromName
  ) where

import Data.Char (isAlpha, isAlphaNum, isUpper)
import Data.Char (ord, toLower, toUpper)
import Data.Bits (xor)
import Data.Word (Word64)
import Data.List (nub, sort)
import GHC.Types.Name
  ( Name
  , isWiredInName
  , nameModule_maybe
  , nameOccName
  )
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Occurrence
  ( OccName
  , isSymOcc
  , isVarNameSpace
  , occNameSpace
  )
import GHC.Unit.Types (moduleName)
import Language.Haskell.Syntax.Module.Name (moduleNameString)

data NameOrigin
  = InternalName String
  | ExternalName String
  | LocalName
  | WiredInName
  deriving (Eq, Show)

data NameTransform = NameTransform
  { transformOriginalModule :: String
  , transformOriginalOccurrence :: String
  , transformGeneratedIdentifier :: String
  }
  deriving (Eq, Show)

data GeneratedNameCategory
  = VarIdentifier
  | ConstructorIdentifier
  | VariableOperator
  | ConstructorOperator

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
    [] -> detectNameTransformConflict rest

isGeneratedNameConflict :: NameTransform -> NameTransform -> Bool
isGeneratedNameConflict left right =
  transformGeneratedIdentifier left == transformGeneratedIdentifier right
    && ( transformOriginalModule left /= transformOriginalModule right
          || transformOriginalOccurrence left /= transformOriginalOccurrence right
       )

generatedIdentifier :: String -> String -> NameTransform
generatedIdentifier sourceModuleName occurrenceName =
  NameTransform
    { transformOriginalModule = sourceModuleName
    , transformOriginalOccurrence = occurrenceName
    , transformGeneratedIdentifier = generatedName sourceModuleName occurrenceName (categoryFromSpelling occurrenceName)
    }

generatedIdentifierFromName :: [String] -> Name -> Maybe NameTransform
generatedIdentifierFromName internalModules name =
  case classifyName internalModules name of
    InternalName moduleNameValue ->
      let originalOccName = nameOccName name
       in Just
            NameTransform
              { transformOriginalModule = moduleNameValue
              , transformOriginalOccurrence = occNameString originalOccName
              , transformGeneratedIdentifier =
                  generatedName
                    moduleNameValue
                    (occNameString originalOccName)
                    (categoryFromOccName originalOccName)
              }
    _ -> Nothing

generatedName :: String -> String -> GeneratedNameCategory -> String
generatedName sourceModuleName occurrenceName category =
  case category of
    VarIdentifier ->
      lowerIdentifier identifierBase
    ConstructorIdentifier ->
      upperIdentifier identifierBase
    VariableOperator ->
      "!" ++ operatorBase
    ConstructorOperator ->
      ":!" ++ operatorBase
  where
    identifierBase =
      sanitizeIdentifier sourceModuleName ++ "_" ++ sanitizeIdentifier occurrenceName
    operatorBase =
      encodeSymbolNumber (stableHashString (sourceModuleName ++ "\0" ++ occurrenceName))

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
categoryFromOperatorSpelling _ = VariableOperator

sanitizeIdentifier :: String -> String
sanitizeIdentifier =
  ensureLeadingAlpha . concatMap sanitizeChar

sanitizeChar :: Char -> String
sanitizeChar char
  | isAlphaNum char = [char]
  | otherwise = "_u" ++ show (ord char) ++ "_"

ensureLeadingAlpha :: String -> String
ensureLeadingAlpha [] = "generated"
ensureLeadingAlpha value@(first : _)
  | isAlpha first = value
  | otherwise = "generated_" ++ value

lowerIdentifier :: String -> String
lowerIdentifier [] = "generated"
lowerIdentifier (first : rest) = toLower first : rest

upperIdentifier :: String -> String
upperIdentifier [] = "Generated"
upperIdentifier (first : rest) = toUpper first : rest

stableHashString :: String -> Word64
stableHashString =
  foldl hashStep 14695981039346656037
  where
    hashStep current char =
      (current `xor` fromIntegral (ord char)) * 1099511628211

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
