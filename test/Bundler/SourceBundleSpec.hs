module Bundler.SourceBundleSpec where

import Bundler.Cabal
  ( ExecutableInfo (..)
  , PackageInfo (..)
  )
import Bundler.Error (BundleError, renderBundleError)
import Bundler.GHC (loadExecutableModules)
import Bundler.SourceBundle (generateSourceBundle)
import Control.Exception (finally)
import Data.List (isInfixOf)
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
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
spec = describe "Bundler.SourceBundle" $ do
  around withTempPackageDir $ do
    it "uses Core pruning to remove unused definitions and retain entry dependencies" $ \packageDir -> do
      writeFixturePackage packageDir
      loaded <- shouldRightRender (loadExecutableModules (packageInfo packageDir) (executableInfo packageDir))
      source <- shouldRightRender (generateSourceBundle (packageInfo packageDir) (executableInfo packageDir) loaded)

      source `shouldSatisfy` ("module Main (main) where" `isInfixOf`)
      source `shouldSatisfy` ("fixture_u46_Entry_main" `isInfixOf`)
      source `shouldSatisfy` ("fixture_u46_Entry_used" `isInfixOf`)
      source `shouldSatisfy` (not . ("fixture_u46_Entry_unused" `isInfixOf`))
      source `shouldSatisfy` (not . ("import qualified Fixture.Entry" `isInfixOf`))
      source `shouldSatisfy` (not . (packageDir `isInfixOf`))
      compileGeneratedSource packageDir source

    it "bundles CPP-selected and Template Haskell declarations end to end" $ \packageDir -> do
      writeThCppFixturePackage packageDir
      let info = thCppExecutableInfo packageDir
          pkg = (packageInfo packageDir) {packageExecutables = [info]}
      loaded <- shouldRightRender (loadExecutableModules pkg info)
      source <- shouldRightRender (generateSourceBundle pkg info loaded)

      source `shouldSatisfy` (not . ("missingCppBranch" `isInfixOf`))
      compileGeneratedSourceWith packageDir ["template-haskell"] source

    it "keeps explicit Prelude boundaries for semantic-sensitive extensions" $ \packageDir -> do
      writePreludeBoundaryFixturePackage packageDir
      let info = preludeBoundaryExecutableInfo packageDir
          pkg = (packageInfo packageDir) {packageExecutables = [info]}
      loaded <- shouldRightRender (loadExecutableModules pkg info)
      source <- shouldRightRender (generateSourceBundle pkg info loaded)

      source `shouldSatisfy` ("semantic-sensitive extension retained globally: NoImplicitPrelude" `isInfixOf`)
      source `shouldSatisfy` ("semantic-sensitive extension retained globally: RebindableSyntax" `isInfixOf`)
      compileGeneratedSource packageDir source

writeFixturePackage :: FilePath -> IO ()
writeFixturePackage packageDir = do
  createDirectoryIfMissing True (packageDir </> "app")
  createDirectoryIfMissing True (packageDir </> "src" </> "Fixture")
  writeFile
    (packageDir </> "app" </> "Main.hs")
    ( unlines
        [ "module Main (P.main) where"
        , "import Fixture.Entry as P (main)"
        ]
    )
  writeFile
    (packageDir </> "src" </> "Fixture" </> "Entry.hs")
    ( unlines
        [ "module Fixture.Entry where"
        , "main :: IO ()"
        , "main = print used"
        , "used :: Int"
        , "used = 1"
        , "unused :: Int"
        , "unused = 2"
        ]
    )

writeThCppFixturePackage :: FilePath -> IO ()
writeThCppFixturePackage packageDir = do
  createDirectoryIfMissing True (packageDir </> "app")
  writeFile
    (packageDir </> "app" </> "Main.hs")
    ( unlines
        [ "{-# LANGUAGE CPP #-}"
        , "{-# LANGUAGE TemplateHaskell #-}"
        , "module Main (main) where"
        , "import qualified Prelude"
        , "#if defined(BUNDLER_CPP_TEST)"
        , "cppValue :: Prelude.Int"
        , "cppValue = 1"
        , "#else"
        , "cppValue :: Prelude.Int"
        , "cppValue = missingCppBranch"
        , "#endif"
        , "$([d| thValue :: Prelude.Int"
        , "      thValue = 41"
        , "    |])"
        , "main :: Prelude.IO ()"
        , "main = Prelude.print (cppValue Prelude.+ thValue)"
        ]
    )

writePreludeBoundaryFixturePackage :: FilePath -> IO ()
writePreludeBoundaryFixturePackage packageDir = do
  createDirectoryIfMissing True (packageDir </> "app")
  writeFile
    (packageDir </> "app" </> "Main.hs")
    ( unlines
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# LANGUAGE RebindableSyntax #-}"
        , "module Main (main) where"
        , "import qualified Prelude"
        , "fromInteger :: Prelude.Integer -> Prelude.Int"
        , "fromInteger = Prelude.fromInteger"
        , "main :: Prelude.IO ()"
        , "main = Prelude.putStrLn \"ok\""
        ]
    )

compileGeneratedSource :: FilePath -> String -> IO ()
compileGeneratedSource packageDir =
  compileGeneratedSourceWith packageDir []

compileGeneratedSourceWith :: FilePath -> [String] -> String -> IO ()
compileGeneratedSourceWith packageDir packageNames source = do
  let path = packageDir </> "Bundled.hs"
      packageArgs = concatMap (\packageName -> ["-package", packageName]) packageNames
  writeFile path source
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode "ghc" (["-fforce-recomp", "-fno-code", path] ++ packageArgs) ""
  exitCode `shouldBe` ExitSuccess
  stderr `shouldBe` ""

withTempPackageDir :: (FilePath -> IO a) -> IO a
withTempPackageDir action = do
  root <- makeTempDirectory
  action root `finally` removePathForcibly root

makeTempDirectory :: IO FilePath
makeTempDirectory = do
  tmp <- getTemporaryDirectory
  (path, handle) <- openTempFile tmp "oj-hs-bundler-source-fixture"
  hClose handle
  removeFile path
  createDirectory path
  pure path

packageInfo :: FilePath -> PackageInfo
packageInfo packageDir =
  PackageInfo
    { packageRoot = packageDir
    , packageCabalFile = packageDir </> "fixture.cabal"
    , packageName = "fixture"
    , packageDisplayName = "fixture-0.0.0.0"
    , packagePathsModuleName = "Paths_fixture"
    , packageVersionNumbers = [0, 0, 0, 0]
    , packageLibrarySourceDirs = [packageDir </> "src"]
    , packageLibraryDependencyPackageNames = []
    , packageExecutables = [executableInfo packageDir]
    }

executableInfo :: FilePath -> ExecutableInfo
executableInfo packageDir =
  ExecutableInfo
    { executableName = "fixture"
    , executableMainPath = packageDir </> "app" </> "Main.hs"
    , executableSourceDirs = [packageDir </> "app"]
    , executableDependencies = []
    , executableDependencyPackageNames = ["base"]
    , executableDefaultExtensions = []
    , executableCompilerOptions = []
    }

thCppExecutableInfo :: FilePath -> ExecutableInfo
thCppExecutableInfo packageDir =
  (executableInfo packageDir)
    { executableDependencyPackageNames = ["base", "template-haskell"]
    , executableDefaultExtensions = ["CPP", "TemplateHaskell"]
    , executableCompilerOptions = ["-DBUNDLER_CPP_TEST"]
    }

preludeBoundaryExecutableInfo :: FilePath -> ExecutableInfo
preludeBoundaryExecutableInfo packageDir =
  (executableInfo packageDir)
    { executableDefaultExtensions = ["NoImplicitPrelude", "RebindableSyntax"]
    }

shouldRightRender :: IO (Either BundleError a) -> IO a
shouldRightRender action = do
  result <- action
  case result of
    Right value -> pure value
    Left err -> expectationFailure (renderBundleError err) >> pure (error "unreachable")
