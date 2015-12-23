import Control.Monad
import Data.List
import System.Directory
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo

main = defaultMainWithHooks simpleUserHooks { postBuild = postBuildHook }
    where postBuildHook _ _ _ info = do
            putStrLn "Deploying main.js..."
            case componentsConfigs info of
                (CExeName name, _, _):_ -> do
                    let src = intercalate "/" [buildDir info, name, name++".jsexe", "all.js"]
                        dest = "./main.js"
                    exists <- doesFileExist src
                    if exists then copyFile src dest >> putStrLn (src ++ " copied to " ++ dest)
                              else putStrLn $ "ERROR: " ++ src ++ " does not exist"
                _ -> putStrLn "ERROR: path to the binary cannot be obtained"
