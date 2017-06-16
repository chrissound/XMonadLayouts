{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MiddleColumn where

import           Control.Monad
import           XMonad
import qualified XMonad.StackSet as W

-- Example: MiddleColumn 0.25 1 0.040 0.25
data MiddleColumn a = MiddleColumn {
  splitRatio        :: Float, -- width ratio of side columns
  middleColumnCount :: Int, -- number of windows in middle column
  deltaIncrement    :: Float,
  middleColumnSpecialTwoRatio :: Float -- ratio of window height when two windows are in the middle column
  } deriving (Show, Read)

instance LayoutClass MiddleColumn a where
  description _ = "MiddleColumn"
  pureLayout (MiddleColumn sr mcc _ mctRatio) screenRec s = zip ws (recs $ length ws) where
    (middleRec:leftRec:rightRec:[]) = mainSplit sr screenRec
    ws = W.integrate s
    middleRecs = if (mcc == 2)
      -- If there are two windows in the "middle column", make the larger window the master
      then (if (mctRatio >= 0.5) then id else reverse) . (\(m1,m2) -> [m1,m2]) $ splitVerticallyBy mctRatio middleRec
      else splitVertically mcc middleRec
    recs wl | wl <= 3    = middleRecs ++ [leftRec, rightRec]
            | otherwise  = middleRecs ++ (splitVertically lLength leftRec) ++ reverse (splitVertically rLength rightRec) where
                (lLength, rLength) = splitDiscrete (wl - mcc)
                splitDiscrete a = (b, a - b) where
                  b = (quot a 2)

  pureMessage l@(MiddleColumn sr mcc deltaInc _) m = msum [
    fmap resize     (fromMessage m),
    fmap incmastern (fromMessage m)
    ]
    where
      resize Expand = l {splitRatio = (min 0.5 $ sr + deltaInc)}
      resize Shrink = l {splitRatio = (max 0 $ sr - deltaInc)}
      incmastern (IncMasterN x) = l { middleColumnCount = max 0 (mcc+x) }

mainSplit :: Float -> Rectangle -> [Rectangle]
mainSplit f (Rectangle sx sy sw sh) = [m, l, r]
  where
    splitW = floor $ fromIntegral sw * f
    l = Rectangle sx sy splitW sh
    m = Rectangle (sx + fromIntegral splitW) sy (sw - (2 * fromIntegral splitW)) sh
    r = Rectangle ((fromIntegral sw) - (fromIntegral splitW)) sy splitW sh
