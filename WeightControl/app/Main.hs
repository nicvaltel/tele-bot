module Main (main) where

import Lib (runBot)
import System.Environment (getArgs)

defaultEnvFile :: FilePath 
defaultEnvFile = "app-db-config.env"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runBot defaultEnvFile
    [filePath] -> runBot filePath
    _ -> putStrLn " Usage: weight-control-exe [env-file-path.env]\n By defalut path is ./app-db-config.env"
