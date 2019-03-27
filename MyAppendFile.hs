module MyAppendFile where

import System.IO

myAppendFile :: FilePath -> String -> IO ()
myAppendFile f s = do
  withFile f AppendMode $ \h -> do
    hPutStrLn h s
