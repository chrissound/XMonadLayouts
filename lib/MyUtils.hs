module MyUtils where

import XMonad
-- import XMonad.StackSet
import XMonad.StackSet as W
-- import MiddleColumn
import XMonad.Actions.TagWindows

-- modifyLayout :: (l -> l) -> X ()
-- modifyLayout f = do
--   s <- get -- >>= screen . current
--   wId <- get >>= return . tag . workspace . current . windowset
--   --updateLayout wId Nothing
--   modLayout wId (\x -> case x of
--                     MiddleColumn _ _ _ _ _ _ _ _ _ _-> x)
--   pure ()

-- modLayout :: WorkspaceId -> (Layout Window -> Layout Window) -> X ()
-- modLayout i f =
--   runOnWorkspaces $ \ww ->
--     return $
--     if tag ww == i
--       then ww {layout = f (layout ww)}
--       else ww


getWindowCount :: X Int
getWindowCount = length . W.integrate' . W.stack . W.workspace . W.current . windowset <$> get

getScreenRes :: X Rectangle
getScreenRes = screenRect . W.screenDetail . W.current . windowset <$> get

currentWindow :: X (Maybe Window)
currentWindow = withWindowSet (pure . W.peek)

toggleTag :: String -> X ()
toggleTag s = do
  currentWindow >>= \case
    Just x ->
      hasTag s x >>= \case
        True -> delTag s x
        False -> addTag s x
    Nothing -> pure ()
