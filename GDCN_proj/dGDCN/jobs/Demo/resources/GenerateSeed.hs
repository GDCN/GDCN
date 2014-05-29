
import System.Random

import qualified Data.ByteString.Lazy as BS
import Data.Binary

generate :: String -> IO ()
generate name = do
    seed <- randomIO :: IO Int
    BS.writeFile (name ++ ".raw") (encode seed)
