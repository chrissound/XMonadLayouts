{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS -Wno-incomplete-patterns #-}

module XMonad.Layout.MasterOverlay where

import Data.List (elemIndex)
import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.StackSet

data MasterOverlay a =
  MasterOverlay (Maybe Window)
  deriving (Show, Read)

data MasterOverLayToggleFocus =
  MasterOverLayToggleFocus

instance Message (MasterOverLayToggleFocus)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item : b)
  where
    (a, (_:b)) = splitAt n ls

instance LayoutModifier MasterOverlay Window where
  modifyLayout (MasterOverlay (Just x)) ss r = do
    (a, b) <- runLayout ss r
    let a' =
          case a of
            allWins@((_, mr):_) ->
              case (elemIndex x $ integrate' . stack $ ss) of
                Just x' -> replaceAtIndex x' (fst $ allWins !! x', mr) allWins
    return (reverse a', b)
  modifyLayout _ ss r = runLayout ss r
  handleMess (MasterOverlay x) m =
    case (fromMessage m) of
      Just MasterOverLayToggleFocus ->
        case x of
          Nothing ->
            withWindowSet $
            (\ss ->
               case peek ss of
                 Just w -> pure $ Just . MasterOverlay . Just $ w)
          _ -> pure $ Just (MasterOverlay Nothing)
      Nothing -> pure Nothing
