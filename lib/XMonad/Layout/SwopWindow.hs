{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS -Wno-incomplete-patterns #-}

module XMonad.Layout.SwopWindow where

-- import Data.List (elemIndex)
-- import XMonad
-- import XMonad.Layout.LayoutModifier
-- import XMonad.StackSet
-- import XMonad.StackSet as W
-- import Types
-- import WindowColumn
-- import MyUtils
-- import FocusWindow

-- data SwopWindow a = SwopWindow
--   deriving (Show, Read)

-- instance LayoutModifier SwopWindow Window where
--   handleMess (SwopWindow) m =
--     case (fromMessage m) of
--       (Just (SwopWindow' wp)) -> do
--           ws <- getWindowState >>= (return . W.stack . W.workspace . W.current)
--           sr <- getScreenRes
--           case ws of
--             Just ws' -> do
--               r <- pure $ (snd) <$> (layoutRectangles l sr ws')
--               case (windowPositionToStacksetIndex wp r) of
--                 Just i -> do
--                   swopWindowToMaster i
--                   return Nothing
--                 Nothing -> error "???"
--             Nothing -> pure Nothing
--       _ -> pure $ Nothing
