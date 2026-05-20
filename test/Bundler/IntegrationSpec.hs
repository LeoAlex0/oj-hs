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
spec = describe "bundler integration" $ do
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

    it "bootstraps bundler deterministically" $ \outputDir -> do
      firstSource <- bundleExecutable outputDir "bundler"
      firstSource `shouldSatisfy` (not . containsBundlerEnvironmentValue [outputDir])
      firstSource `shouldSatisfy` ("{-# LANGUAGE PackageImports #-}" `isInfixOf`)
      firstSource `shouldSatisfy` ("import qualified \"ghc\" GHC.Core" `isInfixOf`)
      firstSource `shouldSatisfy` (not . ("bundler_internal_opaque_either :: Prelude.String" `isInfixOf`))
      compileBundledSourceForExecutable outputDir "bundler.hs" "bundler"
      bundledBundler <-
        compileBundledExecutableForExecutable
          outputDir
          "bundler.hs"
          "bundler-bootstrap"
          "bundler"
      let secondOutputPath = outputDir </> "bundler-second.hs"
      (exitCode, stdout, stderr) <-
        readProcessWithExitCode
          bundledBundler
          ["--exec", "bundler", "-o", secondOutputPath]
          ""
      exitCode `shouldBe` ExitSuccess
      stdout `shouldBe` ""
      stderr `shouldBe` ""
      secondSource <- readFile secondOutputPath
      secondSource `shouldBe` firstSource

bundleExecutable :: FilePath -> String -> IO String
bundleExecutable outputDir executableName = do
  let outputPath = outputDir </> executableName ++ ".hs"
  result <- runBundler (BundleOptions (Just executableName) (Just outputPath) ".")
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
  shouldExitSuccessfully ("ghc -fforce-recomp -fno-code " ++ outputPath) exitCode _stdout stderr
  stderr `shouldBe` ""

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
  shouldExitSuccessfully ("ghc -fforce-recomp -O0 " ++ sourcePath) exitCode _stdout stderr
  stderr `shouldBe` ""
  pure executablePath

shouldExitSuccessfully :: String -> ExitCode -> String -> String -> IO ()
shouldExitSuccessfully command exitCode stdout stderr =
  case exitCode of
    ExitSuccess -> pure ()
    _ ->
      expectationFailure
        ( unlines
            [ "command failed: " ++ command
            , "exit code: " ++ show exitCode
            , "stdout:"
            , stdout
            , "stderr:"
            , stderr
            ]
        )

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
