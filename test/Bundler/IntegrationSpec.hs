module Bundler.IntegrationSpec where

import           Bundler           (runBundler)
import           Bundler.Cabal     (ExecutableInfo (..), PackageInfo (..),
                                    readPackageInfo, selectExecutable)
import           Bundler.Options   (BundleOptions (..))
import           Control.Exception (finally)
import           Data.List         (isInfixOf, nub)
import           System.Directory  (createDirectory, getTemporaryDirectory,
                                    removeFile, removePathForcibly)
import           System.Exit       (ExitCode (ExitSuccess))
import           System.FilePath   ((</>))
import           System.IO         (hClose, openTempFile)
import           System.Process    (readProcessWithExitCode)
import           Test.Hspec        (Spec, around, describe, expectationFailure,
                                    it, shouldBe, shouldSatisfy)

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
      compileBundledSourceForExecutable outputDir "custom-setup.hs" "custom-setup"

    it "bundles all-in-one into a standalone Main module" $ \outputDir -> do
      source <- bundleExecutable outputDir "all-in-one"
      source `shouldSatisfy` ("module Main (main) where" `isInfixOf`)
      source `shouldSatisfy` (not . ("import qualified App." `isInfixOf`))
      source `shouldSatisfy` (not . containsBundlerEnvironmentValue [outputDir])
      compileBundledSourceForExecutable outputDir "all-in-one.hs" "all-in-one"

    it "bootstraps haskell-bundler deterministically" $ \outputDir -> do
      firstSource <- bundleExecutable outputDir "haskell-bundler"
      firstSource `shouldSatisfy` (not . containsBundlerEnvironmentValue [outputDir])
      firstSource `shouldSatisfy` ("{-# LANGUAGE PackageImports #-}" `isInfixOf`)
      firstSource `shouldSatisfy` ("import qualified \"ghc\" GHC.Core" `isInfixOf`)
      firstSource `shouldSatisfy` (not . ("bundler_internal_opaque_either :: Prelude.String" `isInfixOf`))
      compileBundledSourceWithCabalExec outputDir "haskell-bundler.hs"
      bundledBundler <-
        compileBundledExecutableForExecutable
          outputDir
          "haskell-bundler.hs"
          "haskell-bundler-bootstrap"
          "haskell-bundler"
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

compileBundledSourceForExecutable :: FilePath -> FilePath -> String -> IO ()
compileBundledSourceForExecutable outputDir fileName executableName = do
  packageNames <- bundledCompilePackageNames executableName
  compileBundledSourceWithPackages outputDir fileName packageNames

compileBundledSourceWithPackages :: FilePath -> FilePath -> [String] -> IO ()
compileBundledSourceWithPackages outputDir fileName packageNames = do
  let outputPath = outputDir </> fileName
      packageArgs = concatMap (\packageName -> ["-package", packageName]) packageNames
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode "ghc" (["-fforce-recomp", "-fno-code", outputPath] ++ packageArgs) ""
  exitCode `shouldBe` ExitSuccess
  stderr `shouldBe` ""

compileBundledSourceWithCabalExec :: FilePath -> FilePath -> IO ()
compileBundledSourceWithCabalExec outputDir fileName = do
  let outputPath = outputDir </> fileName
  (exitCode, _stdout, _stderr) <-
    readProcessWithExitCode
      "cabal"
      ["exec", "ghc", "--", "--make", "-fforce-recomp", "-fno-code", outputPath]
      ""
  exitCode `shouldBe` ExitSuccess

containsBundlerEnvironmentValue :: [FilePath] -> String -> Bool
containsBundlerEnvironmentValue paths source =
  any
    (`isInfixOf` source)
    (filter (not . null) paths)

compileBundledExecutableForExecutable :: FilePath -> FilePath -> FilePath -> String -> IO FilePath
compileBundledExecutableForExecutable outputDir fileName outputExecutableName sourceExecutableName = do
  let sourcePath = outputDir </> fileName
      executablePath = outputDir </> outputExecutableName
  packageNames <- bundledCompilePackageNames sourceExecutableName
  let packageArgs = concatMap (\packageName -> ["-package", packageName]) packageNames
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode
      "ghc"
      (["-fforce-recomp", "-O0", sourcePath, "-o", executablePath] ++ packageArgs)
      ""
  exitCode `shouldBe` ExitSuccess
  stderr `shouldBe` ""
  pure executablePath

bundledCompilePackageNames :: String -> IO [String]
bundledCompilePackageNames executableName = do
  packageInfoValue <- shouldRight (readPackageInfo ".")
  executableInfoValue <- shouldRightPure (selectExecutable (Just executableName) packageInfoValue)
  pure
    ( filter
        (/= packageName packageInfoValue)
        ( nub
            ( executableDependencyPackageNames executableInfoValue
                ++ packageLibraryDependencyPackageNames packageInfoValue
            )
        )
    )

shouldRight :: Show err => IO (Either err a) -> IO a
shouldRight action = do
  result <- action
  shouldRightPure result

shouldRightPure :: Show err => Either err a -> IO a
shouldRightPure result =
  case result of
    Right value -> pure value
    Left err    -> expectationFailure (show err) >> pure (error "unreachable")

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
