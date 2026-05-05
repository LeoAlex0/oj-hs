module Bundler.Flatten
  ( CandidateModule (..)
  , ModuleSegment (..)
  , renderCandidateModule
  ) where

import           Data.List (intercalate, sort)

data CandidateModule
  = CandidateModule
      { candidateLanguagePragmas :: [String]
      , candidateImports         :: [String]
      , candidateSegments        :: [ModuleSegment]
      , candidateEntryBinding    :: String
      }
  deriving (Eq, Show)

data ModuleSegment
  = ModuleSegment
      { segmentName         :: String
      , segmentDeclarations :: [String]
      }
  deriving (Eq, Show)

renderCandidateModule :: CandidateModule -> String
renderCandidateModule candidate =
  unlines
    ( map renderLanguagePragma (sort (candidateLanguagePragmas candidate))
        ++ ["module Main (main) where", ""]
        ++ map renderQualifiedImport (sort (candidateImports candidate))
        ++ ["" | not (null (candidateImports candidate))]
        ++ concatMap renderSegment (candidateSegments candidate)
        ++ ["main :: IO ()", "main = " ++ candidateEntryBinding candidate]
    )

renderLanguagePragma :: String -> String
renderLanguagePragma extension =
  "{-# LANGUAGE " ++ extension ++ " #-}"

renderQualifiedImport :: String -> String
renderQualifiedImport moduleName =
  "import qualified " ++ moduleName

renderSegment :: ModuleSegment -> [String]
renderSegment segment =
  ["", "-- " ++ segmentName segment]
    ++ lines (intercalate "\n" (segmentDeclarations segment))
    ++ [""]
