module Prime where

import GDCN.Trusted.Data.Binary
import Data.ByteString.Lazy (ByteString)

--  

run :: [ByteString] -> (ByteString, String)
run (h:_) = let num = decode h :: Integer -- Reads input of task
                result = num + 100
            in (encode result, show result)
