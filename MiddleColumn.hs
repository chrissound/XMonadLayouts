{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MiddleColumn where

import           Control.Monad
import           XMonad
import qualified XMonad.StackSet as W

data ModifySideContainer = IncrementLeftColumnContainer | IncrementRightColumnContainer | ResetColumnContainer deriving Typeable
instance Message ModifySideContainer

getMiddleColumnSaneDefault :: Int -> Float -> MiddleColumn a
getMiddleColumnSaneDefault mColumnCount mTwoRatio = MiddleColumn 0.25 mColumnCount 0.04 mTwoRatio 0 0

-- Example: MiddleColumn 0.25 1 0.040 0.25
data MiddleColumn a = MiddleColumn {
  splitRatio        :: Float, -- width ratio of side columns
  middleColumnCount :: Int, -- number of windows in middle column
  deltaIncrement    :: Float,
  middleTwoRatio    :: Float, -- ratio of window height when two windows are in the middle column,
  leftContainerCount :: Int,
  rightContainerCount :: Int
  } deriving (Show, Read)


-- If zero then return no rectangles
splitVerticallyFixed :: Int -> Rectangle -> [Rectangle]
splitVerticallyFixed 0 _ = []
splitVerticallyFixed c r = splitVertically c r

getRecsWithSideContainment :: Rectangle -> Rectangle -> Int -> Int ->  Int -> ([Rectangle], [Rectangle])
-- Show window on left if it's the only window
getRecsWithSideContainment lRec rRec 0 0 1 = ([lRec], []) 
-- divide equally between left and right
getRecsWithSideContainment lRec rRec 0 0 totalCount =
  ( splitVerticallyFixed lCount lRec
  , reverse (splitVerticallyFixed rCount rRec)
  ) where
    (lCount, rCount) = splitDiscrete (totalCount)
    splitDiscrete a = (b, a - b) where
      b = (quot a 2)
-- divide with a max count on left or right
getRecsWithSideContainment lRec rRec leftMax rightMax totalCount = (\(i, j) -> (i, reverse j)) $ if (leftMax > 0)
  then ( splitVerticallyFixed leftMax lRec
       , splitVerticallyFixed (totalCount - leftMax) rRec
       )
  else ( splitVerticallyFixed (totalCount - rightMax) lRec
       , splitVerticallyFixed rightMax rRec
       )

instance LayoutClass MiddleColumn a where
  description _ = "MiddleColumn"
  doLayout l@(MiddleColumn _ mcc _ _ lContainerCount rContainerCount) r s   = do
    let sideColumnWindowCount = (length $ W.integrate s) - mcc
    let l'  = if (lContainerCount > 0)
          then l {
            leftContainerCount = min sideColumnWindowCount lContainerCount
            , rightContainerCount = - (min sideColumnWindowCount lContainerCount)
            }
          else if (rContainerCount > 0) then
            l {
            leftContainerCount = - (min sideColumnWindowCount rContainerCount)
            , rightContainerCount = min sideColumnWindowCount rContainerCount
            }
          else
            l
    return (pureLayout l' r s, Just l')
  pureLayout (MiddleColumn sRatio mcc _ mctRatio lContainerCount rContainerCount) screenRec s = zip ws (recs $ length ws) where
    (middleRec:leftRec:rightRec:[]) = mainSplit sRatio screenRec
    ws = W.integrate s
    middleRecs = if (mcc == 2)
      -- If there are two windows in the "middle column", make the larger window the master
      then (if (mctRatio >= 0.5) then id else reverse) . (\(m1,m2) -> [m1,m2]) $ splitVerticallyBy mctRatio middleRec
      else splitVertically mcc middleRec
    recs wl = middleRecs ++ leftInnerRecs ++ rightInnerRecs where
      (leftInnerRecs, rightInnerRecs) = getRecsWithSideContainment leftRec rightRec lContainerCount rContainerCount ((wl) - mcc)
    -- recs wl | wl <= 3    = middleRecs ++ [leftRec, rightRec]
    --         | otherwise  = middleRecs ++ (splitVertically lLength leftRec) ++ reverse (splitVertically rLength rightRec) where
    --             (lLength, rLength) = splitDiscrete (wl - mcc)
    --             splitDiscrete a = (b, a - b) where
    --               b = (quot a 2)
  pureMessage l@(MiddleColumn sRatio mcc _ _ leftCount rightCount) m = msum [
    fmap resize     (fromMessage m),
    fmap incmastern (fromMessage m),
    fmap incSideContainer (fromMessage m)
    ]
    where
      incSideContainer IncrementLeftColumnContainer = l
        { leftContainerCount = leftCount + 1, rightContainerCount = rightCount - 1}
      incSideContainer IncrementRightColumnContainer = l
        { leftContainerCount = leftCount - 1, rightContainerCount = rightCount + 1}
      incSideContainer ResetColumnContainer = l
        { leftContainerCount = 0, rightContainerCount = 0}
      resize Expand = l {splitRatio = (min 0.5 $ sRatio + 0.04)}
      resize Shrink = l {splitRatio = (max 0 $ sRatio - 0.04)}
      incmastern (IncMasterN x) = l { middleColumnCount = max 0 (mcc+x) }

mainSplit :: Float -> Rectangle -> [Rectangle]
mainSplit f (Rectangle sx sy sw sh) = [m, l, r]
  where
    splitW = floor $ fromIntegral sw * f
    l = Rectangle sx sy splitW sh
    m = Rectangle (sx + fromIntegral splitW) sy (sw - (2 * fromIntegral splitW)) sh
    r = Rectangle ((fromIntegral sw) - (fromIntegral splitW)) sy splitW sh
