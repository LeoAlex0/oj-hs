module Bundler.Options
  ( BundleOptions (..)
  , bundleOptionsParser
  , parseBundleOptions
  ) where

import           Options.Applicative (Parser, ParserInfo, execParser, fullDesc,
                                      header, help, helper, info, long, metavar,
                                      optional, progDesc, short, showDefault,
                                      strOption, switch, value)

data BundleOptions
  = BundleOptions
      { optExecutable :: Maybe String
      , optOutput     :: Maybe FilePath
      , optPackageDir :: FilePath
      , optCompactNames  :: Bool
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
        <> header "bundler"
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
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "PATH"
              <> help "Path for the generated source. Defaults to stdout."
          )
      )
    <*> strOption
      ( long "package-dir"
          <> metavar "DIR"
          <> value "."
          <> showDefault
          <> help "Package directory containing the .cabal file."
      )
    <*> switch
      ( long "compact-names"
          <> help "Use short hash-based generated names instead of readable module/occurrence names."
      )

(<**>) :: Parser a -> Parser (a -> b) -> Parser b
(<**>) = flip (<*>)
