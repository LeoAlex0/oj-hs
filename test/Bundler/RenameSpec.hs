module Bundler.RenameSpec where

import           Bundler.Rename   (NameStyle (CompactNames),
                                   NameTransform (transformGeneratedIdentifier, transformOriginalModule),
                                   detectNameTransformConflict,
                                   generatedIdentifier,
                                   generatedIdentifierWithStyle)
import           Data.Char        (isLower, isUpper)
import           Data.Maybe       (isJust)
import           System.Directory (getTemporaryDirectory, removeFile)
import           System.Exit      (ExitCode (ExitSuccess))
import           System.IO        (hClose, hPutStr, openTempFile)
import           System.Process   (readProcessWithExitCode)
import           Test.Hspec       (Spec, describe, it, shouldBe, shouldNotBe,
                                   shouldSatisfy)

spec :: Spec
spec = describe "Bundler.Rename" $ do
  describe "generatedIdentifier" $ do
    it "generates varid names for lowercase identifiers" $ do
      generated "Data.Text" "value" `shouldSatisfy` startsWith isLower

    it "keeps module and occurrence names readable by default" $ do
      generated "Data.Text" "value" `shouldBe` "v_Data_46_Text_value"

    it "uses stable compact identifiers on request" $ do
      let valueName = generated "Data.Text" "value"
          compactValueName = generatedCompact "Data.Text" "value"
      generatedCompact "Data.Text" "value" `shouldBe` compactValueName
      generatedCompact "Other.Module" "value" `shouldNotBe` compactValueName
      compactValueName `shouldSatisfy` ((< 16) . length)
      compactValueName `shouldNotBe` valueName

    it "generates conid names for uppercase identifiers" $ do
      generated "Data.Text" "Value" `shouldSatisfy` startsWith isUpper

    it "does not let identifier names start with digits" $ do
      generated "1.Data" "2value" `shouldSatisfy` startsWith isLower

    it "preserves varsym spelling as an operator" $ do
      generated "Data.Text" "+>" `shouldSatisfy` isVariableOperator

    it "preserves consym spelling as a constructor operator" $ do
      generated "Data.Text" ":+>" `shouldSatisfy` isConstructorOperator

    it "generates operator names usable in fixity declarations" $ do
      let valueOperator = generated "Data.FingerTree" "><"
          constructorOperator = generated "Data.FingerTree" ":<"
      compileHaskellSource
        ( unlines
            [ "{-# LANGUAGE TypeOperators #-}"
            , "module RenameFixity where"
            , "infixl 5 " ++ valueOperator ++ ", " ++ constructorOperator
            , "data Pair a b = a " ++ constructorOperator ++ " b"
            , "(" ++ valueOperator ++ ") :: Int -> Int -> Int"
            , "x " ++ valueOperator ++ " y = x + y"
            ]
        )

    it "detects generated identifier conflicts" $ do
      let left = generatedIdentifier "A.B" "value"
          right = left {transformOriginalModule = "Other.Module"}
      detectNameTransformConflict [left, right] `shouldSatisfy` isJust

generated :: String -> String -> String
generated moduleName occurrenceName =
  transformGeneratedIdentifier (generatedIdentifier moduleName occurrenceName)

generatedCompact :: String -> String -> String
generatedCompact moduleName occurrenceName =
  transformGeneratedIdentifier (generatedIdentifierWithStyle CompactNames moduleName occurrenceName)

startsWith :: (Char -> Bool) -> String -> Bool
startsWith predicate (first : _) = predicate first
startsWith _ []                  = False

isVariableOperator :: String -> Bool
isVariableOperator value@(first : _) =
  first /= ':' && all isOperatorChar value
isVariableOperator [] = False

isConstructorOperator :: String -> Bool
isConstructorOperator (':' : rest) =
  not (null rest) && all isOperatorChar rest
isConstructorOperator _ = False

isOperatorChar :: Char -> Bool
isOperatorChar char =
  char `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

compileHaskellSource :: String -> IO ()
compileHaskellSource source = do
  tmp <- getTemporaryDirectory
  (path, handle) <- openTempFile tmp "oj-hs-rename-fixity.hs"
  hPutStr handle source
  hClose handle
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode "ghc" ["-fforce-recomp", "-fno-code", path] ""
  removeFile path
  exitCode `shouldBe` ExitSuccess
  stderr `shouldBe` ""
