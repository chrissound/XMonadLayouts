{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MiddleColumn where

import           Control.Monad
import           XMonad
import qualified XMonad.StackSet as W

data MiddleColumn a = MiddleColumn {
  splitRatio        :: Float,
  middleColumnCount :: Int,
  deltaIncrement    :: Float
  } deriving (Show, Read)

instance LayoutClass MiddleColumn a where
  description _ = "MiddleColumn"
  pureLayout (MiddleColumn sr mcc _) screenRec s = zip ws (recs $ length ws) where
    (middleRec:leftRec:rightRec:[]) = middleSplit sr screenRec
    ws = W.integrate s
    middleRecs = splitVertically mcc middleRec
    recs wl | wl <= 3    = middleRecs ++ [leftRec, rightRec]
            | otherwise  = middleRecs ++ (splitVertically lLength leftRec) ++ (splitVertically rLength rightRec) where
                (lLength, rLength) = splitDiscrete (wl - mcc)
                splitDiscrete a = (b, a - b) where
                  b = (quot a 2)

  pureMessage l@(MiddleColumn sr mcc deltaInc) m = msum [
    fmap resize     (fromMessage m),
    fmap incmastern (fromMessage m)
    ]
    where
      resize Expand = l {splitRatio = (min 0.5 $ sr + deltaInc)}
      resize Shrink = l {splitRatio = (max 0 $ sr - deltaInc)}
      incmastern (IncMasterN x) = l { middleColumnCount = max 0 (mcc+x) }

middleSplit :: Float -> Rectangle -> [Rectangle]
middleSplit f (Rectangle sx sy sw sh) = [m, l, r]
  where
    splitW = floor $ fromIntegral sw * f
    l = Rectangle sx sy splitW sh
    m = Rectangle (sx + fromIntegral splitW) sy (sw - (2 * fromIntegral splitW)) sh
    r = Rectangle ((fromIntegral sw) - (fromIntegral splitW)) sy splitW sh
