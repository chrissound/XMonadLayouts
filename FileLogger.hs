{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS -Wno-incomplete-patterns #-}

module FileLogger where

-- import Prelude hiding (appendFile)
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock
-- import Data.String.Conversions
-- import Control.Monad.IO.Class
import Data.Time.Format
import MyAppendFile
import Text.Pretty.Simple
import Data.String.Conversions
-- import Control.DeepSeq

logToTmpFile :: String -> IO ()
logToTmpFile = myAppendFile "/tmp/xmonad.log" . (++ "\n")

-- logM :: (MonadIO m, Show a) => a -> m ()
-- logM s = do
--   x <- liftIO $ getCurrentTime
--   return $! unsafePerformIO $! do
--       logToTmpFile $ myFormatUtcTime (x) ++ ": " ++ show s
logM :: Applicative m => a -> m ()
logM = const $ pure ()

log' :: (Show a) => a -> ()
log' s = unsafePerformIO $! do
      logToTmpFile $ show s

logM' :: String -> a -> a
logM' = const $ (id)
-- logM' :: (Show a) => String -> a -> a
-- logM' s' s = unsafePerformIO $! do
--       x <- getCurrentTime
--       logToTmpFile $ myFormatUtcTime (x) ++ ": ("++s'++") :" ++ show s
--       pure s

logExtra  :: (Show a) => String -> a -> ()
logExtra a s = unsafePerformIO $! do
      logToTmpFile $ a ++ show s

logid  :: (Show a) => a -> a
logid a = do
  unsafePerformIO $! do
      logToTmpFile $ cs $ pShow a
      pure a

myFormatUtcTime :: UTCTime -> String
myFormatUtcTime = formatTime defaultTimeLocale "%H:%M:%S"
