{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WindowColumn where

import           XMonad
import WindowCoordinates
import Data.List
import Types

windowPositionToWindow :: WindowPosition -> [Rectangle] -> Rectangle
windowPositionToWindow wp r = case (wColumn wp, wDirection wp) of
  (Types.Left, Up) -> topLeftSort r        !! (wIndex wp - 1)
  (Types.Left, Down) -> bottomLeftSort r   !! (wIndex wp - 1)
  (Types.Right, Up) -> topRightSort r      !! (wIndex wp - 1)
  (Types.Right, Down) -> bottomRightSort r !! (wIndex wp - 1)
  _ -> error "windowPosition can only be determined for left/right"

windowPositionToStacksetIndex :: WindowPosition -> [Rectangle] -> Maybe Int
windowPositionToStacksetIndex wp r = elemIndex (windowPositionToWindow wp r) r

normalizeWindowPosition :: WindowPosition -> MiddleColumnRecs-> Int
normalizeWindowPosition wp (m,l,_) = case wColumn wp of
  Types.Left -> length m + index
  Types.Middle -> index
  Types.Right -> length (m++l) + index
  where
    index = wIndex wp - 1

normalizeSwopWindowPosition :: SwopWindow' -> [Window] -> MiddleColumnRecs-> Maybe Int
normalizeSwopWindowPosition v w r = case v of
  SwopWindowIndex x -> Just x
  SwopWindowWindow x -> elemIndex x w
  SwopWindow' x -> Just $ normalizeWindowPosition x r
