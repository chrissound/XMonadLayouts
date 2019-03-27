module MyUtils where

import XMonad
import XMonad.Operations
import XMonad.StackSet
import MiddleColumn

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
