
import Langermann

import Control.Exception
import qualified Data.ByteString.Lazy as BS
import Data.Binary
import System.Environment
import System.Exit

main :: IO ()
main = do args <- getArgs
          case args of
              (resultFile : _) -> do
                  resultData <- BS.readFile resultFile
                  result <- catch (evaluate (decode resultData :: Vector))
                                  (\e -> (e :: ErrorCall) `seq` do putStrLn "Deceitful result!"
                                                                   exitSuccess)
                  let (x, y) = result
                  if x >= 0 && x <= 10 && y >= 0 && y <= 10
                     then do putStrLn (show $ langermann result)
                             exitSuccess
                     else do putStrLn "Deceitful result"
                             exitSuccess
              _ -> exitFailure
