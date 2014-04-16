
import Data.Binary
import qualified Data.ByteString.Lazy as BS

makeN :: Integer -> IO ()
makeN n = do
    let x = encode n
    BS.writeFile (show n ++ ".raw") x
