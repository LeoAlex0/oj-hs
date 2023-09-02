{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import App.Import
import App.Run
import Options.Applicative.Simple
import qualified Paths_oj_hs
import RIO.Process

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_oj_hs.version)
      "Header for command line arguments"
      "Program description, also for command line arguments"
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
          <*> strOption (long "package" <> help "package.yaml to pack")
      )
      empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
            }
     in runRIO app run
