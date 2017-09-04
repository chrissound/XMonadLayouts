{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MiddleColumn where

import           Control.Monad
import           XMonad
import qualified XMonad.StackSet as W
import FocusWindow
import Data.List (sortBy)
import Data.Function (on)

data ModifySideContainer = IncrementLeftColumnContainer | IncrementRightColumnContainer | ResetColumnContainer deriving Typeable
instance Message ModifySideContainer

data FocusSideColumnWindow n = FocusLeft n | FocusRight n deriving Typeable
instance Message (FocusSideColumnWindow Int)

data SwopSideColumnWindow n = SwopLeft n | SwopRight n deriving Typeable
instance Message (SwopSideColumnWindow Int)

getMiddleColumnSaneDefault :: Int -> Float -> (Float,Float,Float) -> MiddleColumn a
getMiddleColumnSaneDefault mColumnCount mTwoRatio mThreeRatio = MiddleColumn 0.25 mColumnCount 0.04 mTwoRatio mThreeRatio 0 0

data MiddleColumnEnum = LColumn | MColumn | RColumn

-- Example: MiddleColumn 0.25 1 0.040 0.25
data MiddleColumn a = MiddleColumn {
  splitRatio        :: Float, -- width ratio of side columns
  middleColumnCount :: Int, -- number of windows in middle column
  deltaIncrement    :: Float,
  middleTwoRatio    :: Float, -- ratio of window height when two windows are in the middle column,
  middleThreeRatio    :: (Float,Float,Float), -- ratio of window height when two windows are in the middle column,
  leftContainerCount :: Int,
  rightContainerCount :: Int
  } deriving (Show, Read)


-- If zero then return no rectangles
splitVerticallyFixed :: Int -> Rectangle -> [Rectangle]
splitVerticallyFixed 0 _ = []
splitVerticallyFixed c r = splitVertically c r

xAccumulateRecatangle :: [Rectangle] -> [Rectangle]
xAccumulateRecatangle ([]) = []
xAccumulateRecatangle (r1:[]) = [r1]
xAccumulateRecatangle (r1:r2:[]) = r1 : [r2 {rect_x = floor $ (fromIntegral $ rect_x r1) + (fromIntegral $ rect_width r1 :: Float)}]
xAccumulateRecatangle (r1:r2:r3) = do
  let [ar1, ar2] = xAccumulateRecatangle (r1 : [r2])
  ar1 : (xAccumulateRecatangle $ ar2 : r3)

splitHorizontallyByRatios :: [Float] -> Rectangle -> [Rectangle]
splitHorizontallyByRatios ratios mainR@(Rectangle _ _ w _) = do
  let widthSet = fmap (\ratio -> mainR { rect_width = floor $ fromIntegral w * ratio}) ratios
  xAccumulateRecatangle widthSet where

splitVerticallyByRatios :: [Float] -> Rectangle -> [Rectangle]
splitVerticallyByRatios f = fmap mirrorRect . splitHorizontallyByRatios f . mirrorRect

getRecsWithSideContainment :: Rectangle -> Rectangle -> Int -> Int ->  Int -> ([Rectangle], [Rectangle])
-- Show window on left if it's the only window
getRecsWithSideContainment lRec _ 0 0 1 = ([lRec], [])
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
  doLayout l r s   = do
    let mcc = middleColumnCount l
    let lContainerCount = leftContainerCount l
    let rContainerCount = rightContainerCount l
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
  pureLayout l screenRec s = zip ws (recs $ length ws) where
    mcc = middleColumnCount l
    mctRatio = middleTwoRatio l
    mc3Ratio = middleThreeRatio l
    sRatio = splitRatio l
    lContainerCount = leftContainerCount l
    rContainerCount = rightContainerCount l
    (middleRec:leftRec:rightRec:[]) = mainSplit sRatio screenRec
    ws = W.integrate s
    middleRecs = 
      -- If there are two windows in the "middle column", make the larger window the master
      if (mcc == 2) then
        reverse . sortBy (compare `on` rect_height) $ (\(m1,m2) -> [m1,m2]) $ splitVerticallyBy mctRatio middleRec
      else if (mcc == 3) then
        reverse . sortBy (compare `on` rect_height) $ splitVerticallyByRatios ((\(m1,m2,m3) -> [m1,m2,m3]) mc3Ratio) middleRec
      else
        splitVertically mcc middleRec
    recs wl = middleRecs ++ leftInnerRecs ++ rightInnerRecs where
      (leftInnerRecs, rightInnerRecs) = getRecsWithSideContainment leftRec rightRec lContainerCount rContainerCount ((wl) - mcc)
  pureMessage l m = msum [
    fmap resize     (fromMessage m),
    fmap incmastern (fromMessage m),
    fmap incSideContainer (fromMessage m)
    ]
    where
      sRatio = splitRatio l
      mcc = middleColumnCount l
      leftCount = leftContainerCount l
      rightCount = rightContainerCount l
      incSideContainer IncrementLeftColumnContainer = l
        { leftContainerCount = leftCount + 1, rightContainerCount = rightCount - 1}
      incSideContainer IncrementRightColumnContainer = l
        { leftContainerCount = leftCount - 1, rightContainerCount = rightCount + 1}
      incSideContainer ResetColumnContainer = l
        { leftContainerCount = 0, rightContainerCount = 0}
      resize Expand = l {splitRatio = (min 0.5 $ sRatio + 0.04)}
      resize Shrink = l {splitRatio = (max 0 $ sRatio - 0.04)}
      incmastern (IncMasterN x) = l { middleColumnCount = max 0 (mcc+x) }
  handleMessage l m = do
    let leftWindowOffset = (middleColumnCount l - 1)
    -- Not sure how to avoid this nested case.
    case (fromMessage m :: Maybe (FocusSideColumnWindow Int)) of
      (Just (FocusLeft n)) -> do
        windows $ focusWindow $ n + leftWindowOffset
        return Nothing
      (Just (FocusRight n)) -> do
        windows $ focusWindow $ negate $ n + leftWindowOffset
        return Nothing
      Nothing ->
        case (fromMessage m :: Maybe (SwopSideColumnWindow Int)) of
        (Just (SwopLeft n)) -> do
          swopWindowToMaster $ n + leftWindowOffset
          return Nothing
        (Just (SwopRight n)) -> do
          windows $ focusWindow (negate n)
          swopWindowToMaster $ negate n
          return Nothing
        Nothing -> return $ pureMessage l m

mainSplit :: Float -> Rectangle -> [Rectangle]
mainSplit f (Rectangle sx sy sw sh) = [m, l, r]
  where
    splitW = floor $ fromIntegral sw * f
    l = Rectangle sx sy splitW sh
    m = Rectangle (sx + fromIntegral splitW) sy (sw - (2 * fromIntegral splitW)) sh
    r = Rectangle ((fromIntegral sw) - (fromIntegral splitW)) sy splitW sh
