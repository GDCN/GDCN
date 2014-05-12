
import Control.Exception
import qualified Data.ByteString.Lazy as BS
import Data.Binary
import System.Environment
import System.Exit


main :: IO ()
main = do args <- getArgs
          case args of
              (resultFile : (inputFile : _)) -> do
                  input <- BS.readFile inputFile
                  result <- BS.readFile resultFile
                  let cn = decode input :: Integer 
                  rn <- catch (evaluate (decode result :: Integer))
                              (\e -> (e :: ErrorCall) `seq` do putStrLn "BAD!"
                                                               exitSuccess)
                  if rn <= cn + 100
                     then do putStrLn (show (rn - cn))
                             exitSuccess
                     else do putStrLn "BAD!"
                             exitSuccess
              _ -> exitFailure

