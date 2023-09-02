import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup

postHook :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postHook args flags desc info = do
  putStrLn "already post hook"

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks {postBuild = postHook}

-- main = defaultMain
