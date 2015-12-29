import Control.Monad
import Data.List
import System.Directory
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo

main = defaultMainWithHooks simpleUserHooks { postBuild = postBuildHook }
    where postBuildHook _ _ _ info = do
            let files = ["rts", "lib", "out"]
            putStrLn $ "Deploying {" ++ intercalate "," files ++ "}.js..."
            case componentsConfigs info of
                (CExeName name, _, _):_ -> do
                    let exeDir = intercalate "/" [buildDir info, name, name++".jsexe"]
                        destDir = "js"
                    createDirectoryIfMissing False destDir
                    forM_ files $ \f -> do
                        let src = exeDir ++ "/" ++ f ++ ".js"
                            dest = destDir ++ "/" ++ f ++ ".js"
                        exists <- doesFileExist src
                        if exists then copyFile src dest >> putStrLn (src ++ " copied to " ++ dest)
                                  else putStrLn $ "ERROR: " ++ src ++ " does not exist"
                _ -> putStrLn "ERROR: path to the binary cannot be obtained"
