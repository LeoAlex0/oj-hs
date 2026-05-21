module Bundler.SourceBundleSpec where

import           Bundler.Cabal        (ExecutableInfo (..), PackageInfo (..),
                                       readPackageInfo, selectExecutable)
import           Bundler.Error        (BundleError, renderBundleError)
import           Bundler.GHC          (LoadedGhcModules (..),
                                       loadExecutableModules)
import           Bundler.Rename       (NameStyle (ReadableNames),
                                       generatedIdentifier,
                                       transformGeneratedIdentifier)
import           Bundler.SourceBundle (generateSourceBundle)
import           Control.Exception    (finally)
import           Data.List            (isInfixOf)
import           System.Directory     (createDirectory,
                                       createDirectoryIfMissing,
                                       getTemporaryDirectory, removeFile,
                                       removePathForcibly)
import           System.Exit          (ExitCode (ExitSuccess))
import           System.FilePath      ((</>))
import           System.IO            (hClose, openTempFile)
import           System.Process       (readProcessWithExitCode)
import           Test.Hspec           (Spec, around, describe,
                                       expectationFailure, it, shouldBe,
                                       shouldSatisfy)

spec :: Spec
spec = describe "Bundler.SourceBundle" $ do
  around withTempPackageDir $ do
    it "uses Core pruning to remove unused definitions and retain entry dependencies" $ \packageDir -> do
      writeFixturePackage packageDir
      loaded <- shouldRightRender (loadExecutableModules (packageInfo packageDir) (executableInfo packageDir))
      source <- shouldRightRender (generateSourceBundle ReadableNames (packageInfo packageDir) (executableInfo packageDir) loaded)

      loadedUnitPackageNames loaded `shouldSatisfy` any ((== "base") . snd)
      source `shouldSatisfy` ("module Main (main) where" `isInfixOf`)
      source `shouldSatisfy` (generated "Fixture.Entry" "main" `isInfixOf`)
      source `shouldSatisfy` (generated "Fixture.Entry" "used" `isInfixOf`)
      source `shouldSatisfy` (not . (generated "Fixture.Entry" "unused" `isInfixOf`))
      source `shouldSatisfy` (not . ("import qualified Fixture.Entry" `isInfixOf`))
      source `shouldSatisfy` (not . ("bundler_internal_opaque_either :: Prelude.String" `isInfixOf`))
      source `shouldSatisfy` (not . (packageDir `isInfixOf`))
      compileGeneratedSource packageDir source

    it "bundles CPP-selected and Template Haskell declarations end to end" $ \packageDir -> do
      writeThCppFixturePackage packageDir
      let info = thCppExecutableInfo packageDir
          pkg = (packageInfo packageDir) {packageExecutables = [info]}
      loaded <- shouldRightRender (loadExecutableModules pkg info)
      source <- shouldRightRender (generateSourceBundle ReadableNames pkg info loaded)

      source `shouldSatisfy` (not . ("missingCppBranch" `isInfixOf`))
      compileGeneratedSourceWith packageDir ["template-haskell"] source

    it "keeps explicit Prelude boundaries for semantic-sensitive extensions" $ \packageDir -> do
      writePreludeBoundaryFixturePackage packageDir
      let info = preludeBoundaryExecutableInfo packageDir
          pkg = (packageInfo packageDir) {packageExecutables = [info]}
      loaded <- shouldRightRender (loadExecutableModules pkg info)
      source <- shouldRightRender (generateSourceBundle ReadableNames pkg info loaded)

      source `shouldSatisfy` ("{-# LANGUAGE NoImplicitPrelude #-}" `isInfixOf`)
      source `shouldSatisfy` ("{-# LANGUAGE RebindableSyntax #-}" `isInfixOf`)
      compileGeneratedSource packageDir source

    it "handles retained synthetic Paths directory functions" $ \packageDir -> do
      writePathsDataFixturePackage packageDir
      let info = pathsDataExecutableInfo packageDir
          pkg = (packageInfo packageDir) {packageExecutables = [info]}
      loaded <- shouldRightRender (loadExecutableModules pkg info)
      source <- shouldRightRender (generateSourceBundle ReadableNames pkg info loaded)

      source `shouldSatisfy` (not . (packageDir `isInfixOf`))
      compileGeneratedSource packageDir source

    it "preserves executable Cabal default extensions in generated source" $ \packageDir -> do
      writeExecutableDefaultExtensionFixturePackage packageDir
      let info = executableLambdaCaseInfo packageDir
          pkg = (packageInfo packageDir) {packageExecutables = [info]}
      loaded <- shouldRightRender (loadExecutableModules pkg info)
      source <- shouldRightRender (generateSourceBundle ReadableNames pkg info loaded)

      source `shouldSatisfy` ("{-# LANGUAGE LambdaCase #-}" `isInfixOf`)
      compileGeneratedSource packageDir source

    it "deduplicates equivalent pragmas after merging modules" $ \packageDir -> do
      writeDuplicatePragmaFixturePackage packageDir
      let info =
            (executableInfo packageDir)
              { executableDefaultExtensions = ["ScopedTypeVariables"]
              }
          pkg =
            (packageInfo packageDir)
              { packageLibraryDefaultExtensions = ["DeriveGeneric"]
              , packageExecutables = [info]
              }
      loaded <- shouldRightRender (loadExecutableModules pkg info)
      source <- shouldRightRender (generateSourceBundle ReadableNames pkg info loaded)

      countLine "{-# LANGUAGE DeriveGeneric #-}" source `shouldBe` 1
      countLine "{-# LANGUAGE ScopedTypeVariables #-}" source `shouldBe` 1
      countLine "{-# OPTIONS_GHC -Wno-unused-top-binds #-}" source `shouldBe` 1
      source `shouldSatisfy` (not . ("DeriveGeneric          " `isInfixOf`))
      compileGeneratedSource packageDir source

    it "uses library default extensions while loading and rendering" $ \packageDir -> do
      writeLibraryDefaultExtensionFixturePackage packageDir
      let info = executableInfo packageDir
          pkg =
            (packageInfo packageDir)
              { packageLibraryDefaultExtensions = ["LambdaCase"]
              , packageExecutables = [info]
              }
      loaded <- shouldRightRender (loadExecutableModules pkg info)
      source <- shouldRightRender (generateSourceBundle ReadableNames pkg info loaded)

      source `shouldSatisfy` ("{-# LANGUAGE LambdaCase #-}" `isInfixOf`)
      compileGeneratedSource packageDir source

    it "uses Cabal cpp-options when loading a package description" $ \packageDir -> do
      writeCabalCppOptionsFixturePackage packageDir
      pkg <- shouldRightRender (readPackageInfo packageDir)
      info <- shouldRightRenderPure (selectExecutable (Just "fixture") pkg)
      loaded <- shouldRightRender (loadExecutableModules pkg info)
      source <- shouldRightRender (generateSourceBundle ReadableNames pkg info loaded)

      source `shouldSatisfy` (not . ("missingCppOptionBranch" `isInfixOf`))
      compileGeneratedSource packageDir source

    it "resolves main-is from later executable source dirs" $ \packageDir -> do
      writeMultiSourceMainFixturePackage packageDir
      pkg <- shouldRightRender (readPackageInfo packageDir)
      info <- shouldRightRenderPure (selectExecutable (Just "fixture") pkg)

      executableMainPath info `shouldBe` packageDir </> "app" </> "Main.hs"

    it "does not rewrite external names inside literals" $ \packageDir -> do
      writeLiteralRewriteFixturePackage packageDir
      let info = executableInfo packageDir
          pkg = (packageInfo packageDir) {packageExecutables = [info]}
      loaded <- shouldRightRender (loadExecutableModules pkg info)
      source <- shouldRightRender (generateSourceBundle ReadableNames pkg info loaded)

      source `shouldSatisfy` ("\"putStrLn\"" `isInfixOf`)
      source `shouldSatisfy` (not . ("\"Prelude.putStrLn\"" `isInfixOf`))
      compileGeneratedSource packageDir source

    it "selects the executable entry when another internal module defines main" $ \packageDir -> do
      writeInternalHelperMainFixturePackage packageDir
      loaded <- shouldRightRender (loadExecutableModules (packageInfo packageDir) (executableInfo packageDir))
      source <- shouldRightRender (generateSourceBundle ReadableNames (packageInfo packageDir) (executableInfo packageDir) loaded)

      source `shouldSatisfy` (("main = " ++ generated "Fixture.Entry" "main") `isInfixOf`)
      compileGeneratedSource packageDir source

generated :: String -> String -> String
generated moduleName occurrenceName =
  transformGeneratedIdentifier (generatedIdentifier moduleName occurrenceName)

countLine :: String -> String -> Int
countLine expected =
  length . filter (== expected) . lines

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
        , "main = print (used + length opaqueHelperLiteral)"
        , "used :: Int"
        , "used = 1"
        , "opaqueHelperLiteral :: String"
        , "opaqueHelperLiteral = \"bundler_internal_opaque_either\""
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

writePathsDataFixturePackage :: FilePath -> IO ()
writePathsDataFixturePackage packageDir = do
  createDirectoryIfMissing True (packageDir </> "app")
  writeFile
    (packageDir </> "app" </> "Main.hs")
    ( unlines
        [ "module Main (main) where"
        , "import qualified Paths_fixture"
        , "main :: IO ()"
        , "main = Paths_fixture.getDataFileName \"asset.txt\" >>= putStrLn"
        ]
    )

writeExecutableDefaultExtensionFixturePackage :: FilePath -> IO ()
writeExecutableDefaultExtensionFixturePackage packageDir = do
  createDirectoryIfMissing True (packageDir </> "app")
  writeFile
    (packageDir </> "app" </> "Main.hs")
    ( unlines
        [ "module Main (main) where"
        , "main :: IO ()"
        , "main = print (select (Just 1))"
        , "select :: Maybe Int -> Int"
        , "select = \\case"
        , "  Just value -> value"
        , "  Nothing -> 0"
        ]
    )

writeDuplicatePragmaFixturePackage :: FilePath -> IO ()
writeDuplicatePragmaFixturePackage packageDir = do
  createDirectoryIfMissing True (packageDir </> "app")
  createDirectoryIfMissing True (packageDir </> "src" </> "Fixture")
  writeFile
    (packageDir </> "app" </> "Main.hs")
    ( unlines
        [ "{-# LANGUAGE DeriveGeneric              #-}"
        , "{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}"
        , "{-# OPTIONS_GHC -Wno-unused-top-binds        #-}"
        , "module Main (main) where"
        , "import Fixture.Entry (entry)"
        , "main :: IO ()"
        , "main = print entry"
        ]
    )
  writeFile
    (packageDir </> "src" </> "Fixture" </> "Entry.hs")
    ( unlines
        [ "{-# LANGUAGE DeriveGeneric          #-}"
        , "{-# OPTIONS_GHC -Wno-unused-top-binds #-}"
        , "module Fixture.Entry where"
        , "entry :: Int"
        , "entry = 1"
        ]
    )

writeLibraryDefaultExtensionFixturePackage :: FilePath -> IO ()
writeLibraryDefaultExtensionFixturePackage packageDir = do
  createDirectoryIfMissing True (packageDir </> "app")
  createDirectoryIfMissing True (packageDir </> "src" </> "Fixture")
  writeFile
    (packageDir </> "app" </> "Main.hs")
    ( unlines
        [ "module Main (main) where"
        , "import Fixture.Entry (entry)"
        , "main :: IO ()"
        , "main = print (entry (Just 1))"
        ]
    )
  writeFile
    (packageDir </> "src" </> "Fixture" </> "Entry.hs")
    ( unlines
        [ "module Fixture.Entry where"
        , "entry :: Maybe Int -> Int"
        , "entry = \\case"
        , "  Just value -> value"
        , "  Nothing -> 0"
        ]
    )

writeCabalCppOptionsFixturePackage :: FilePath -> IO ()
writeCabalCppOptionsFixturePackage packageDir = do
  createDirectoryIfMissing True (packageDir </> "app")
  writeFile
    (packageDir </> "fixture.cabal")
    ( unlines
        [ "cabal-version: 2.2"
        , "name: fixture"
        , "version: 0.0.0.0"
        , "executable fixture"
        , "  main-is: Main.hs"
        , "  hs-source-dirs: app"
        , "  build-depends: base"
        , "  default-language: Haskell2010"
        , "  default-extensions: CPP"
        , "  cpp-options: -DBUNDLER_CPP_OPTION"
        ]
    )
  writeFile
    (packageDir </> "app" </> "Main.hs")
    ( unlines
        [ "module Main (main) where"
        , "#if defined(BUNDLER_CPP_OPTION)"
        , "selected :: Int"
        , "selected = 1"
        , "#else"
        , "selected :: Int"
        , "selected = missingCppOptionBranch"
        , "#endif"
        , "main :: IO ()"
        , "main = print selected"
        ]
    )

writeMultiSourceMainFixturePackage :: FilePath -> IO ()
writeMultiSourceMainFixturePackage packageDir = do
  createDirectoryIfMissing True (packageDir </> "empty")
  createDirectoryIfMissing True (packageDir </> "app")
  writeFile
    (packageDir </> "fixture.cabal")
    ( unlines
        [ "cabal-version: 2.2"
        , "name: fixture"
        , "version: 0.0.0.0"
        , "executable fixture"
        , "  main-is: Main.hs"
        , "  hs-source-dirs: empty app"
        , "  build-depends: base"
        , "  default-language: Haskell2010"
        ]
    )
  writeFile
    (packageDir </> "app" </> "Main.hs")
    ( unlines
        [ "module Main (main) where"
        , "main :: IO ()"
        , "main = pure ()"
        ]
    )

writeLiteralRewriteFixturePackage :: FilePath -> IO ()
writeLiteralRewriteFixturePackage packageDir = do
  createDirectoryIfMissing True (packageDir </> "app")
  writeFile
    (packageDir </> "app" </> "Main.hs")
    ( unlines
        [ "module Main (main) where"
        , "main :: IO ()"
        , "main = putStrLn \"putStrLn\""
        ]
    )

writeInternalHelperMainFixturePackage :: FilePath -> IO ()
writeInternalHelperMainFixturePackage packageDir = do
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
        , "import Fixture.Helper (helper)"
        , "main :: IO ()"
        , "main = helper"
        ]
    )
  writeFile
    (packageDir </> "src" </> "Fixture" </> "Helper.hs")
    ( unlines
        [ "module Fixture.Helper where"
        , "helper :: IO ()"
        , "helper = putStrLn \"entry\""
        , "main :: IO ()"
        , "main = putStrLn \"helper\""
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
    , packageLibraryDefaultExtensions = []
    , packageLibraryCompilerOptions = []
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

pathsDataExecutableInfo :: FilePath -> ExecutableInfo
pathsDataExecutableInfo =
  executableInfo

executableLambdaCaseInfo :: FilePath -> ExecutableInfo
executableLambdaCaseInfo packageDir =
  (executableInfo packageDir)
    { executableDefaultExtensions = ["LambdaCase"]
    }

shouldRightRender :: IO (Either BundleError a) -> IO a
shouldRightRender action = do
  result <- action
  shouldRightRenderPure result

shouldRightRenderPure :: Either BundleError a -> IO a
shouldRightRenderPure result =
  case result of
    Right value -> pure value
    Left err -> expectationFailure (renderBundleError err) >> pure (error "unreachable")
