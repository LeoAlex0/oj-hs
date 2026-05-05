module Bundler.Options
  ( BundleOptions (..)
  , bundleOptionsParser
  , parseBundleOptions
  ) where

import           Options.Applicative (Parser, ParserInfo, execParser, fullDesc,
                                      header, help, helper, info, long, metavar,
                                      optional, progDesc, short, showDefault,
                                      strOption, value)

data BundleOptions
  = BundleOptions
      { optExecutable :: Maybe String
      , optOutput     :: FilePath
      , optPackageDir :: FilePath
      }
  deriving (Eq, Show)

parseBundleOptions :: IO BundleOptions
parseBundleOptions = execParser bundleOptionsInfo

bundleOptionsInfo :: ParserInfo BundleOptions
bundleOptionsInfo =
  info
    (bundleOptionsParser <**> helper)
    ( fullDesc
        <> progDesc "Bundle a package executable and its reachable internal modules into one Haskell source file."
        <> header "haskell-bundler"
    )

bundleOptionsParser :: Parser BundleOptions
bundleOptionsParser =
  BundleOptions
    <$> optional
      ( strOption
          ( long "exec"
              <> metavar "NAME"
              <> help "Package executable to bundle. Defaults to the first executable stanza."
          )
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "PATH"
          <> value "bundled.hs"
          <> showDefault
          <> help "Path for the generated single-file Haskell source."
      )
    <*> strOption
      ( long "package-dir"
          <> metavar "DIR"
          <> value "."
          <> showDefault
          <> help "Package directory containing the .cabal file."
      )

(<**>) :: Parser a -> Parser (a -> b) -> Parser b
(<**>) = flip (<*>)
