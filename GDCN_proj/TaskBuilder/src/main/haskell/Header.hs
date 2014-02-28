{-# OPTIONS_GHC -O2 -fpackage-trust #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-} -- If needed use Trustworthy instead

-- NOTE: Each package must be declared trustworthy at compile time with the flag -trust
--       For this to compile these flags need to be added: "-trust base -trust bytestring"

import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import qualified Data.ByteString.Lazy as BS

-- Imports the module X when compiling with "-DMODULE=X" flag
import safe MODULE (run)

-- Force proper typecheck the module's run function
typedRun :: [BS.ByteString] -> (BS.ByteString, String)
typedRun = run

main :: IO ()
main = do args <- getArgs
          case args of
              (outfile:rest) -> do
                  bss <- mapM BS.readFile args
                  let (result, debug) = typedRun bss
                  BS.writeFile outfile result
                  putStr debug
              _ -> do
                  hPutStrLn stderr "Arguments missing"
                  exitWith exitFailure
