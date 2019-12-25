module MyDebug where

-- import FileLogger
-- import Control.Monad.IO.Class
import Debug.Trace

data MyDebug = MyDebugRecs | MyDebugXmonadWin | MyDebugMessage | MyDebugFunctions deriving (Eq, Show)


mdid :: Show a => MyDebug -> a -> a
mdid d = if (elem d debugThese) then (trace $ show d) . traceShowId else id

mdid' :: Show a => MyDebug -> String -> a -> a
-- mdid' d@MyDebugRecs s = trace s $ traceShowId
mdid' d s = if (elem d debugThese) then trace s $ traceShowId else id

debugThese :: [MyDebug]
debugThese = [
  --   MyDebugXmonadWin
  -- , MyDebugFunctions
             ]

------------------

-- mdid :: Show a => MyDebug -> a -> a
-- mdid d@MyDebugRecs = deepseq (log' d) logid
-- mdid _ = id

-- mdid' :: Show a => MyDebug -> String -> a -> a
-- mdid' d s a = deepseq (log' s) $ mdid d a

-- mdid :: (MonadIO m, Show a) => MyDebug -> a -> m a
-- mdid d@MyDebugRecs a = do
--   () <- pure $ log' d
--   pure $ a
-- mdid _ a = pure a

-- mdid' :: (MonadIO m,  Show a) => MyDebug -> String -> a -> m a
-- mdid' d s a = do
--   () <- pure $ log' s
--   mdid d a
