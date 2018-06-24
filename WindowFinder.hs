module WindowFinder where

import XMonad
import qualified XMonad.StackSet as W
import Control.Monad
import Data.Bool  (bool)

findWindows :: (Display -> Window -> IO a) -> (a -> Bool) -> (X [Window])
findWindows f ff' = do
  withWindowSet $ (\ws -> do
    forM (W.allWindows ws)
      (\w -> (withDisplay $ \d -> liftIO $ f d w) >>= \s -> return $ bool [] [w] (ff' s) :: X [Window]
      ) >>= return . join
    )

findWindowsByClass :: String -> X [Window]
findWindowsByClass n = findWindows (\d w -> getClassHint d w >>= return . resClass) ((==) n)
