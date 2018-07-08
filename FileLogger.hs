module FileLogger where

import System.IO.Unsafe (unsafePerformIO)

logToTmpFile :: String -> IO ()
logToTmpFile = appendFile "/tmp/xmonad.log" . (++ "\n")

fuckR :: (Monad m, Show a) => a -> m ()
fuckR s = return $! unsafePerformIO $! do
      logToTmpFile $ show s

fuckRNoM :: (Show a) => a -> ()
fuckRNoM s = unsafePerformIO $! do
      logToTmpFile $ show s

fuckRNoM' :: (Show a) => String -> a -> ()
fuckRNoM' a s = unsafePerformIO $! do
      logToTmpFile $ a ++ show s
