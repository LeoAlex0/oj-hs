module Bundler.GHCSpec where

import           Bundler.Cabal     (ExecutableInfo (..), PackageInfo (..))
import           Bundler.Error     (BundleError, renderBundleError)
import           Bundler.GHC       (loadExecutableModules)
import           Control.Exception (finally)
import           Data.Either       (isRight)
import           Data.List         (isInfixOf)
import           System.Directory  (createDirectory, getTemporaryDirectory,
                                    removeFile, removePathForcibly)
import           System.FilePath   ((</>))
import           System.IO         (hClose, openTempFile)
import           Test.Hspec        (Spec, around, describe, it, shouldSatisfy)

spec :: Spec
spec = describe "Bundler.GHC" $ do
  around withTempPackageDir $ do
    it "runs CPP through the configured GHC parser pipeline" $ \packageDir -> do
      let sourceDir = packageDir </> "app"
      writeFile
        (sourceDir </> "Main.hs")
        ( unlines
            [ "{-# LANGUAGE CPP #-}"
            , "module Main where"
            , "main :: IO ()"
            , "main = print selected"
            , "selected :: Int"
            , "#if defined(BUNDLER_CPP_TEST)"
            , "selected = 1"
            , "#else"
            , "selected = missingName"
            , "#endif"
            ]
        )
      result <-
        loadExecutableModules
          (packageInfo packageDir)
          (executableInfo packageDir ["CPP"] ["-DBUNDLER_CPP_TEST"] [])
      result `shouldSatisfy` isRight

    it "executes Template Haskell splices while typechecking" $ \packageDir -> do
      let sourceDir = packageDir </> "app"
      writeFile
        (sourceDir </> "Main.hs")
        ( unlines
            [ "{-# LANGUAGE TemplateHaskell #-}"
            , "module Main where"
            , "import Language.Haskell.TH"
            , "$(pure [SigD (mkName \"generated\") (ConT ''Int), ValD (VarP (mkName \"generated\")) (NormalB (LitE (IntegerL 1))) []])"
            , "main :: IO ()"
            , "main = print generated"
            ]
        )
      result <-
        loadExecutableModules
          (packageInfo packageDir)
          (executableInfo packageDir ["TemplateHaskell"] [] ["template-haskell"])
      result `shouldSatisfy` isRight

    it "includes Template Haskell failure diagnostics in load errors" $ \packageDir -> do
      let sourceDir = packageDir </> "app"
      writeFile
        (sourceDir </> "Main.hs")
        ( unlines
            [ "{-# LANGUAGE TemplateHaskell #-}"
            , "module Main where"
            , "import Language.Haskell.TH"
            , "$(fail \"th fixture failed\")"
            , "main :: IO ()"
            , "main = pure ()"
            ]
        )
      result <-
        loadExecutableModules
          (packageInfo packageDir)
          (executableInfo packageDir ["TemplateHaskell"] [] ["template-haskell"])
      renderResult result `shouldSatisfy` ("th fixture failed" `isInfixOf`)

withTempPackageDir :: (FilePath -> IO a) -> IO a
withTempPackageDir action = do
  root <- makeTempDirectory
  createDirectory (root </> "app")
  action root `finally` removePathForcibly root

makeTempDirectory :: IO FilePath
makeTempDirectory = do
  tmp <- getTemporaryDirectory
  (path, handle) <- openTempFile tmp "oj-hs-bundler-ghc-fixture"
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
    , packageLibrarySourceDirs = []
    , packageLibraryDependencyPackageNames = []
    , packageLibraryDefaultExtensions = []
    , packageLibraryCompilerOptions = []
    , packageExecutables = []
    }

executableInfo :: FilePath -> [String] -> [String] -> [String] -> ExecutableInfo
executableInfo packageDir extensions options extraDependencies =
  ExecutableInfo
    { executableName = "fixture"
    , executableMainPath = packageDir </> "app" </> "Main.hs"
    , executableSourceDirs = [packageDir </> "app"]
    , executableDependencies = []
    , executableDependencyPackageNames = "base" : extraDependencies
    , executableDefaultExtensions = extensions
    , executableCompilerOptions = options
    }

renderResult :: Either BundleError a -> String
renderResult (Left err) = renderBundleError err
renderResult (Right _)  = ""
