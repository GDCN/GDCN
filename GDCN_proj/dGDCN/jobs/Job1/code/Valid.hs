
-- Sample validifier program for prime
-- It is also more inefficient than the task it compares to, but details :P

import Prime

import Control.Exception
import qualified Data.ByteString.Lazy as BS
import Data.Binary
import Data.List
import System.Environment
import System.Exit


main :: IO ()
main = do args <- getArgs
          case args of
              [resultFile] -> do result <- BS.readFile resultFile
                                 ns <- catch (evaluate (decode result :: [Integer]))
                                             (\e -> (e :: ErrorCall) `seq` do putStrLn "BAD!"
                                                                              exitSuccess)
                                 let ns' = prime 2 (last ns + 1) [] []
                                 if ns `isInfixOf` ns'
                                     then do putStrLn (show $ length ns)
                                             exitSuccess
                                     else do putStrLn "BAD!"
                                             exitSuccess
              _ -> exitFailure

