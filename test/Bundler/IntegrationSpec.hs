module Bundler.IntegrationSpec where

import Bundler (runBundler)
import Bundler.Options (BundleOptions (..))
import Control.Exception (finally)
import Data.List (isInfixOf)
import System.Directory
  ( createDirectory
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  )
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Hspec
  ( Spec
  , around
  , describe
  , expectationFailure
  , it
  , shouldBe
  , shouldSatisfy
  )

spec :: Spec
spec = describe "haskell-bundler integration" $ do
  around withTempPackageDir $ do
    it "bundles luogu-wip into a standalone Main module" $ \outputDir -> do
      source <- bundleExecutable outputDir "luogu-wip"
      source `shouldSatisfy` ("module Main (main) where" `isInfixOf`)
      source `shouldSatisfy` (not . ("import qualified Solution." `isInfixOf`))
      compileBundledSource outputDir "luogu-wip.hs"

    it "bundles codeforces-wip into a standalone Main module" $ \outputDir -> do
      source <- bundleExecutable outputDir "codeforces-wip"
      source `shouldSatisfy` ("module Main (main) where" `isInfixOf`)
      source `shouldSatisfy` (not . ("import qualified Solution." `isInfixOf`))
      source `shouldSatisfy` (not . ("import qualified Data.FingerTree" `isInfixOf`))
      compileBundledSource outputDir "codeforces-wip.hs"

    it "bundles custom-setup into a standalone Main module" $ \outputDir -> do
      source <- bundleExecutable outputDir "custom-setup"
      source `shouldSatisfy` ("module Main (main) where" `isInfixOf`)
      compileBundledSourceWithPackages outputDir "custom-setup.hs" ["Cabal"]

    it "bundles all-in-one into a standalone Main module" $ \outputDir -> do
      source <- bundleExecutable outputDir "all-in-one"
      source `shouldSatisfy` ("module Main (main) where" `isInfixOf`)
      source `shouldSatisfy` (not . ("import qualified App." `isInfixOf`))
      source `shouldSatisfy` (not . containsBuildEnvironmentValue)
      compileBundledSourceWithPackages
        outputDir
        "all-in-one.hs"
        ["rio", "lens", "optparse-simple", "hpack", "ghc-lib-parser"]

    it "bootstraps haskell-bundler deterministically" $ \outputDir -> do
      firstSource <- bundleExecutable outputDir "haskell-bundler"
      firstSource `shouldSatisfy` (not . containsBuildEnvironmentValue)
      bundledBundler <- compileBundledExecutable outputDir "haskell-bundler.hs" "haskell-bundler-bootstrap"
      let secondOutputPath = outputDir </> "haskell-bundler-second.hs"
      (exitCode, stdout, stderr) <-
        readProcessWithExitCode
          bundledBundler
          ["--exec", "haskell-bundler", "--output", secondOutputPath]
          ""
      exitCode `shouldBe` ExitSuccess
      stdout `shouldBe` ""
      stderr `shouldBe` ""
      secondSource <- readFile secondOutputPath
      secondSource `shouldBe` firstSource

bundleExecutable :: FilePath -> String -> IO String
bundleExecutable outputDir executableName = do
  let outputPath = outputDir </> executableName ++ ".hs"
  result <- runBundler (BundleOptions (Just executableName) outputPath ".")
  case result of
    Left err -> expectationFailure (show err) >> pure ""
    Right () -> readFile outputPath

compileBundledSource :: FilePath -> FilePath -> IO ()
compileBundledSource outputDir fileName =
  compileBundledSourceWithPackages outputDir fileName []

compileBundledSourceWithPackages :: FilePath -> FilePath -> [String] -> IO ()
compileBundledSourceWithPackages outputDir fileName packageNames = do
  let outputPath = outputDir </> fileName
      packageArgs = concatMap (\packageName -> ["-package", packageName]) packageNames
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode "ghc" (["-fforce-recomp", "-fno-code", outputPath] ++ packageArgs) ""
  exitCode `shouldBe` ExitSuccess
  stderr `shouldBe` ""

containsBuildEnvironmentValue :: String -> Bool
containsBuildEnvironmentValue source =
  any
    (`isInfixOf` source)
    [ "/home/"
    , "/tmp/"
    , ".git/"
    ]

compileBundledExecutable :: FilePath -> FilePath -> FilePath -> IO FilePath
compileBundledExecutable outputDir fileName executableName = do
  let sourcePath = outputDir </> fileName
      executablePath = outputDir </> executableName
      packageArgs =
        concatMap
          (\packageName -> ["-package", packageName])
          [ "ghc"
          , "Cabal"
          , "optparse-applicative"
          , "containers"
          , "directory"
          , "filepath"
          , "process"
          , "time"
          ]
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode
      "ghc"
      (["-fforce-recomp", "-O0", sourcePath, "-o", executablePath] ++ packageArgs)
      ""
  exitCode `shouldBe` ExitSuccess
  stderr `shouldBe` ""
  pure executablePath

withTempPackageDir :: (FilePath -> IO a) -> IO a
withTempPackageDir action = do
  root <- makeTempDirectory
  action root `finally` removePathForcibly root

makeTempDirectory :: IO FilePath
makeTempDirectory = do
  tmp <- getTemporaryDirectory
  (path, handle) <- openTempFile tmp "oj-hs-bundler-integration"
  hClose handle
  removeFile path
  createDirectory path
  pure path
