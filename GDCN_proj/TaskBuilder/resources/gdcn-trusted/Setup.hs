import System.Environment
import System.Exit

import Distribution.Simple

main :: IO ()
main = do args <- getArgs
          case args of
              [prefix, pkgdb] -> do
                  defaultMainArgs ["configure", "--user", "--libdir=" ++ prefix,
                                   "--package-db=" ++ pkgdb]
                  defaultMainArgs ["build"]
                  defaultMainArgs ["install"]
                  defaultMainArgs ["clean"]
                  exitSuccess
              _ -> do
                  putStrLn "Syntax: runhaskell Setup <install-path> <package-db>"
                  exitFailure
