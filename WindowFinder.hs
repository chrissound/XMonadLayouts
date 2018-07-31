module WindowFinder where

import Control.Monad
import Data.Bool (bool)
import Data.List (isPrefixOf)
import Foreign.C.String
import XMonad
import qualified XMonad.StackSet as W
import XMonad.StackSet
import Data.List (elemIndex)

windowIndex :: Eq a => a -> StackSet i l a s sd -> Maybe Int
windowIndex w s = elemIndex w $ allWindowsInCurrentWorkspace s

findWindows ::
     (WindowSet -> [Window])
  -> (Display -> Window -> IO a)
  -> (a -> Bool)
  -> (X [Window])
findWindows aa f ff' = do
  withWindowSet $
    (\ws -> do
       forM
         (aa ws)
         (\w ->
            (withDisplay $ \d -> liftIO $ f d w) >>= \s ->
              return $ bool [] [w] (ff' s) :: X [Window]) >>=
         return . join)

findWindowsByClass :: String -> X [Window]
findWindowsByClass n =
  findWindows
    W.allWindows
    (\d w -> getClassHint d w >>= return . resClass)
    ((==) n)

findWindowsByTitle :: String -> X [Window]
findWindowsByTitle n =
  findWindows
    W.allWindows
    (\d w -> (getTextProperty d w wM_NAME) >>= peekCString . tp_value)
    ((==) n)

findWindowsInCurrentWorkspaceByTitle :: String -> X [Window]
findWindowsInCurrentWorkspaceByTitle n =
  findWindows
    allWindowsInCurrentWorkspace
    (\d w -> (getTextProperty d w wM_NAME) >>= peekCString . tp_value)
    ((==) n)

findWindowsInCurrentWorkspaceByTitlePrefix :: String -> X [Window]
findWindowsInCurrentWorkspaceByTitlePrefix n =
  findWindows
    allWindowsInCurrentWorkspace
    (\d w -> (getTextProperty d w wM_NAME) >>= peekCString . tp_value)
    (isPrefixOf n)

allWindowsInCurrentWorkspace :: W.StackSet i l a sid sd -> [a]
allWindowsInCurrentWorkspace ss =
  W.integrate' . W.stack . W.workspace . W.current $ ss

allWindowsInWorkspace :: Workspace i l a -> [a]
allWindowsInWorkspace ws =
  W.integrate' . W.stack $ ws
