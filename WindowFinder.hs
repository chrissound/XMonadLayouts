module WindowFinder where

import XMonad
import qualified XMonad.StackSet as W
import Control.Monad
import Data.Bool  (bool)

findWindows :: String -> X [Window]
findWindows name = do
  withWindowSet $ (\ws -> do
    forM (W.allWindows ws)
      (\w -> do
            s <- withDisplay $ \d -> fmap resClass . liftIO $ getClassHint d w
            return $ bool [] [w] (s == name) :: X [Window]
      ) >>= return . join
    )
