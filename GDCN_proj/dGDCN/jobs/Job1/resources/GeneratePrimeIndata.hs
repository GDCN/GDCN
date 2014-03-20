
import Data.Binary
import qualified Data.ByteString.Lazy as BS

makeInterval :: (Integer, Integer) -> IO ()
makeInterval (a, b) = do
    let x = encode (a, b)
    BS.writeFile (show a ++ "_" ++ show b ++ ".raw") x

makeMany :: [Integer] -> IO ()
makeMany (x:(y:xs)) = do
    makeInterval (x+1, y)
    makeMany (y:xs)
makeMany _ = return ()
