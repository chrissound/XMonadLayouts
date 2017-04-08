{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MiddleColumn where

import XMonad
import qualified XMonad.StackSet as W
import Control.Monad

data MiddleColumn a = MiddleColumn {
  splitRatio :: Float
                                   } deriving (Show, Read)
instance LayoutClass MiddleColumn a where
  description _ = "MiddleColumn"
  pureLayout (MiddleColumn sr) screenRec s = zip ws (recs $ length ws) where
    mainRectangles@(middleRec:leftRec:rightRec:[]) = middleSplit sr screenRec
    ws = W.integrate s
    recs wl | wl <= 3    = mainRectangles
            | otherwise  = middleRec : (splitVertically lLength leftRec) ++ (splitVertically rLength rightRec) where
                (lLength, rLength) = splitDiscrete (wl - 1)
                splitDiscrete a = (b, a - b) where
                  b = (quot a 2)

  pureMessage (MiddleColumn sr) m = msum [fmap resize     (fromMessage m)]
    where
      resize Expand  = MiddleColumn (min 1 $ sr + delta)
      resize Shrink  = MiddleColumn (max 0 $ sr - delta)
      delta = 0.015

middleSplit :: Float -> Rectangle -> [Rectangle]
middleSplit f (Rectangle sx sy sw sh) = [m, l, r]
  where
    splitW = floor $ fromIntegral sw * f
    l = Rectangle sx sy splitW sh
    m = Rectangle (sx + fromIntegral splitW) sy (sw - (2 * fromIntegral splitW)) sh
    r = Rectangle ((fromIntegral sw) - (fromIntegral splitW)) sy splitW sh
