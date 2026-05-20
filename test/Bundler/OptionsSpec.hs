module Bundler.OptionsSpec where

import           Bundler.Options     (BundleOptions (..), bundleOptionsParser)
import           Options.Applicative (ParserResult (..), defaultPrefs,
                                      execParserPure, fullDesc, info)
import           Test.Hspec          (Spec, describe, it, shouldBe,
                                      shouldSatisfy)

spec :: Spec
spec = describe "Bundler.Options" $ do
  it "parses executable and output options" $ do
    parseOptions ["--exec", "luogu-wip", "--output", "out.hs"]
      `shouldBe` Right (BundleOptions (Just "luogu-wip") (Just "out.hs") ".")

  it "uses stdout as the default output" $ do
    parseOptions []
      `shouldBe` Right (BundleOptions Nothing Nothing ".")

  it "rejects unknown options through optparse-applicative" $ do
    parseOptions ["--does-not-exist"] `shouldSatisfy` isParseFailure

parseOptions :: [String] -> Either String BundleOptions
parseOptions args =
  case execParserPure defaultPrefs (info bundleOptionsParser fullDesc) args of
    Success options              -> Right options
    Failure failure              -> Left (show failure)
    CompletionInvoked completion -> Left (show completion)

isParseFailure :: Either String BundleOptions -> Bool
isParseFailure (Left _)  = True
isParseFailure (Right _) = False
