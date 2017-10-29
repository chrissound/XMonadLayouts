module FileLogger where

logToTmpFile :: String -> IO ()
logToTmpFile = appendFile "/tmp/xmonad.log" . (++ "\n")
