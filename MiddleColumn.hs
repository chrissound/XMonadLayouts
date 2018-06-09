{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module MiddleColumn where

import Control.Monad
import FocusWindow hiding (traceTraceShowId)
import XMonad
import qualified XMonad.StackSet as W
import XMonad.StackSet (modify')

import Control.Lens
import Data.Foldable
import Data.Function (on)
import Data.List (sortBy)
import Debug.Trace
import Text.Read
import WindowColumn
  ( Column(Left, Middle, Right)
  , SwopSideColumnWindow(..)
  , SwopTo(SwopTo)
  )

traceTraceShowId :: Show a => String -> a -> a
traceTraceShowId x = traceShow x . traceShowId

data ModifySideContainer
  = IncrementLeftColumnContainer
  | IncrementRightColumnContainer
  | ResetColumnContainer
  deriving (Typeable)

instance Message ModifySideContainer

data ModifySideContainerWidth
  = IncrementLeftColumnContainerWidth
  | IncrementRightColumnContainerWidth
  | DecrementLeftColumnContainerWidth
  | DecrementRightColumnContainerWidth
  | ResetColumnContainerWidth
  deriving (Typeable)

instance Message ModifySideContainerWidth

data FocusSideColumnWindow n
  = FocusLeft n
  | FocusRight n
  deriving (Typeable)

instance Message (FocusSideColumnWindow Int)

data SwopSideColumn
  = SwopLeftColumn
  | SwopRightColumn
  | ResetColumn
  deriving (Show, Typeable)

instance Message (SwopSideColumn)

data ModifyLayout =
  ModifyLayout (MiddleColumnModify)

instance Message (MiddleColumnModify)

instance Read SwopSideColumn where
  readPrec = return (ResetColumn)
  readListPrec = readListPrecDefault

data MiddleColumnModify = MiddleColumnModify
  { modifySplitRatio :: Float -> Float
  , modifyMiddleColumnCount :: Int -> Int
  , modifyDeltaIncrement :: Float -> Float
  , modifyMiddleTwoRatio :: Float -> Float
  , modifyMiddleThreeRatio :: (Float, Float, Float) -> (Float, Float, Float) -- ratio of window height when two windows are in the middle column,
  , modifyLeftContainerWidth :: Maybe (Float) -> Maybe (Float)
  , modifyRightContainerWidth :: Maybe (Float) -> Maybe (Float)
  , modifyLeftContainerCount :: Int -> Int
  , modifyRightContainerCount :: Int -> Int
  , modifyColumnSwop :: SwopSideColumn -> SwopSideColumn
  }

modifyMiddleColumn :: MiddleColumnModify -> MiddleColumn a -> MiddleColumn a
modifyMiddleColumn mcm@MiddleColumnModify{..} mc = 
  mc
  { _splitRatio = modifySplitRatio $ _splitRatio mc
  , _middleColumnCount = modifyMiddleColumnCount $ _middleColumnCount mc
  , _deltaIncrement = modifyDeltaIncrement $ _deltaIncrement mc
  , _middleTwoRatio = modifyMiddleTwoRatio $ _middleTwoRatio mc
  , _middleThreeRatio = modifyMiddleThreeRatio $ _middleThreeRatio mc
  , _leftContainerWidth = modifyLeftContainerWidth $ _leftContainerWidth mc
  , _rightContainerWidth = modifyRightContainerWidth $ _rightContainerWidth mc
  , _leftContainerCount = modifyLeftContainerCount $ _leftContainerCount mc
  , _rightContainerCount = modifyRightContainerCount $ _rightContainerCount mc
  , _columnSwop = modifyColumnSwop $ _columnSwop mc
  }

middleColumnModifyId :: MiddleColumnModify
middleColumnModifyId =
  MiddleColumnModify
  { modifySplitRatio = id
  , modifyMiddleColumnCount = id
  , modifyDeltaIncrement = id
  , modifyMiddleTwoRatio = id
  , modifyMiddleThreeRatio = id
  , modifyLeftContainerWidth = id
  , modifyRightContainerWidth = id
  , modifyLeftContainerCount = id
  , modifyRightContainerCount = id
  , modifyColumnSwop = id
  }

getMiddleColumnSaneDefault ::
     Int -> Float -> (Float, Float, Float) -> MiddleColumn a
getMiddleColumnSaneDefault mColumnCount mTwoRatio mThreeRatio =
  MiddleColumn
  { _splitRatio = (0.25 - 1 * (0.04))
  , _middleColumnCount = mColumnCount
  , _deltaIncrement = 0.04
  , _middleTwoRatio = mTwoRatio
  , _middleThreeRatio = mThreeRatio
  , _leftContainerWidth = Nothing
  , _rightContainerWidth = Nothing
  , _leftContainerCount = 2
  , _rightContainerCount = -2
  , _columnSwop = ResetColumn
  }

data MiddleColumnEnum
  = LColumn
  | MColumn
  | RColumn

-- Example: MiddleColumn 0.25 1 0.040 0.25
data MiddleColumn a = MiddleColumn
  { _splitRatio :: Float -- width ratio of side columns
  , _middleColumnCount :: Int -- number of windows in middle column
  , _deltaIncrement :: Float
  , _middleTwoRatio :: Float -- ratio of window height when two windows are in the middle column,
  , _middleThreeRatio :: (Float, Float, Float) -- ratio of window height when two windows are in the middle column,
  , _leftContainerWidth :: Maybe (Float)
  , _rightContainerWidth :: Maybe (Float)
  , _leftContainerCount :: Int
  , _rightContainerCount :: Int
  , _columnSwop :: SwopSideColumn
  } deriving (Show, Read)

makeLenses ''MiddleColumn

-- If zero then return no rectangles
splitVerticallyFixed :: Int -> Rectangle -> [Rectangle]
splitVerticallyFixed 0 _ = []
splitVerticallyFixed c r = splitVertically c r

xAccumulateRecatangle :: [Rectangle] -> [Rectangle]
xAccumulateRecatangle ([]) = []
xAccumulateRecatangle (r1:[]) = [r1]
xAccumulateRecatangle (r1:r2:[]) =
  r1 :
  [ r2
    { rect_x =
        floor $
        (fromIntegral $ rect_x r1) + (fromIntegral $ rect_width r1 :: Float)
    }
  ]
xAccumulateRecatangle (r1:r2:r3) = do
  let [ar1, ar2] = xAccumulateRecatangle (r1 : [r2])
  ar1 : (xAccumulateRecatangle $ ar2 : r3)

splitHorizontallyByRatios :: [Float] -> Rectangle -> [Rectangle]
splitHorizontallyByRatios ratios mainR@(Rectangle _ _ w _) = do
  let widthSet =
        fmap
          (\ratio -> mainR {rect_width = floor $ fromIntegral w * ratio})
          ratios
  xAccumulateRecatangle widthSet
  where


splitVerticallyByRatios :: [Float] -> Rectangle -> [Rectangle]
splitVerticallyByRatios f =
  fmap mirrorRect . splitHorizontallyByRatios f . mirrorRect

getRecsWithSideContainment ::
     Rectangle -> Rectangle -> Int -> Int -> Int -> ([Rectangle], [Rectangle])
-- Show window on left if it's the only window
getRecsWithSideContainment lRec _ 0 0 1 = ([lRec], [])
-- divide equally between left and right
getRecsWithSideContainment lRec rRec 0 0 totalCount =
  (splitVerticallyFixed lCount lRec, reverse (splitVerticallyFixed rCount rRec))
  where
    (lCount, rCount) = splitDiscrete (totalCount)
    splitDiscrete a = (b, a - b)
      where
        b = (quot a 2)
-- divide with a max count on left or right
getRecsWithSideContainment lRec rRec leftMax rightMax totalCount =
  (\(i, j) -> (i, reverse j)) $
  if (leftMax > 0)
    then ( splitVerticallyFixed leftMax lRec
         , splitVerticallyFixed (totalCount - leftMax) rRec)
    else ( splitVerticallyFixed (totalCount - rightMax) lRec
         , splitVerticallyFixed rightMax rRec)

columnSwops :: MiddleColumn a -> [Rectangle] -> [Rectangle]
columnSwops l (middleRec:leftRec:rightRec:[]) =
  case (_columnSwop l) of
    ResetColumn -> [middleRec, leftRec, rightRec]
    SwopLeftColumn -> [leftRec, middleRec, rightRec]
    SwopRightColumn -> [rightRec, leftRec, middleRec]
columnSwops _ r = r

ifNothing :: Maybe a -> a -> Maybe a
ifNothing (Just x) _ = Just x
ifNothing (Nothing) v = Just v

instance LayoutClass MiddleColumn a where
  description _ = "MiddleColumn"
  doLayout l r s = do
    let mcc = _middleColumnCount l
    let lContainerCount = _leftContainerCount l
    let rContainerCount = _rightContainerCount l
    let sideColumnWindowCount = (length $ W.integrate s) - mcc
    let l' =
          if (lContainerCount > 0)
            then l {_leftContainerCount = lcc, _rightContainerCount = -(lcc)}
            else if (rContainerCount > 0)
                   then l
                        { _leftContainerCount = -(rcc)
                        , _rightContainerCount = rcc
                        }
                   else l
          where
            lcc = min sideColumnWindowCount lContainerCount
            rcc = min sideColumnWindowCount rContainerCount
    return (pureLayout l' r s, Just l')
  pureLayout l screenRec s = zip ws (recs $ length ws)
    where
      mcc = _middleColumnCount l
      mctRatio = _middleTwoRatio l
      mc3Ratio = _middleThreeRatio l
      lContainerCount = _leftContainerCount l
      rContainerCount = _rightContainerCount l
      (middleRec:leftRec:rightRec:[]) = mainSplit l screenRec
      ws = W.integrate s
      sortByHeightDesc = reverse . sortBy (compare `on` rect_height)
      middleRecs
      -- If there are two windows in the "middle column", make the larger window the master
       =
        if (mcc == 2)
          then sortByHeightDesc $
               (\(m1, m2) -> [m1, m2]) $ splitVerticallyBy mctRatio middleRec
          else if (mcc == 3)
                 then sortByHeightDesc $
                      splitVerticallyByRatios
                        ((\(m1, m2, m3) -> [m1, m2, m3]) mc3Ratio)
                        middleRec
                 else splitVertically mcc middleRec
      recs wl = middleRecs ++ leftInnerRecs ++ rightInnerRecs
        where
          (leftInnerRecs, rightInnerRecs) =
            getRecsWithSideContainment
              leftRec
              rightRec
              lContainerCount
              rContainerCount
              ((wl) - mcc)
  pureMessage l m =
    msum
      [ fmap resize (fromMessage m)
      , fmap incmastern (fromMessage m)
      , fmap (flip incSideContainer l) (fromMessage m)
      , fmap (incSideContainerWidth l) (fromMessage m)
      , fmap columnSwopAbc (fromMessage m)
      , fmap columnSwopAbc (fromMessage m)
      , fmap (flip modifyMiddleColumn l) (fromMessage m)
      ]
    where
      sRatio = _splitRatio l
      mcc = _middleColumnCount l
      -- column swops
      columnSwopAbc cs = l {_columnSwop = cs}
      resize Expand = l {_splitRatio = (min 0.5 $ sRatio + 0.04)}
      resize Shrink = l {_splitRatio = (max 0 $ sRatio - 0.04)}
      incmastern (IncMasterN x) = l {_middleColumnCount = max 0 (mcc + x)}
  handleMessage l m = do
    ws <- getWindowState >>= (return . W.stack . W.workspace . W.current)
    let windowCount =
          (traceTraceShowId "WindowCount:" $ maybe 0 (length . W.integrate) ws)
    let leftWindowOffset =
          traceTraceShowId "leftWindowOffset:" $ (_middleColumnCount l - 1)
    let possibleMessages =
          [ case (fromMessage m :: Maybe (FocusSideColumnWindow Int)) of
              (Just (FocusLeft n)) ->
                return $ do
                  windows $
                    focusWindow $
                    (traceTraceShowId "FocusLeft:" n) + leftWindowOffset
                  return Nothing
              (Just (FocusRight n)) ->
                return $ do
                  windows $
                    focusWindow $
                    getLastNthWindowIndex
                      (traceTraceShowId "FocusRight:" n)
                      windowCount
                  return Nothing
              _ -> do
                Nothing
          , case (fromMessage m :: Maybe (SwopSideColumnWindow Int)) of
              (Just (SwopLeft n)) ->
                return $ do
                  swopWindowToMaster $ n + leftWindowOffset
                  return Nothing
              (Just (SwopRight n)) ->
                return $ do
                  swopWindowToMaster $ (getLastNthWindowIndex n windowCount)
                  return Nothing
              _ -> Nothing
          , case (fromMessage m :: Maybe (SwopTo)) of
              (Just (SwopTo f t c)) ->
                return $ do
                  let f' =
                        case c of
                          WindowColumn.Left -> f + leftWindowOffset
                          WindowColumn.Right ->
                            getLastNthWindowIndex f windowCount
                          WindowColumn.Middle -> f
                  let modifier = swopStackElements f' t
                  windows $ modify' modifier
                  return $ Just l
              _ -> Nothing
          ]
    case (asum possibleMessages) of
      Just x -> x
      _ -> return $ pureMessage l m

widthDelta :: Float
widthDelta = 0.02

widthInc, widthDec :: Float -> Float
widthInc = (+ widthDelta)

widthDec = flip (-) widthDelta

incSideContainer :: ModifySideContainer -> MiddleColumn l -> MiddleColumn l
incSideContainer IncrementLeftColumnContainer =
  (leftContainerCount +~ 1) . (rightContainerCount -~ 1)
incSideContainer IncrementRightColumnContainer =
  (leftContainerCount -~ 1) . (rightContainerCount +~ 1)
incSideContainer ResetColumnContainer =
  (leftContainerCount .~ 0) . (rightContainerCount .~ 0)

colWidthOver ::
     (Float -> b)
  -> ASetter (MiddleColumn a) t (Maybe Float) (Maybe b)
  -> MiddleColumn a
  -> t
colWidthOver f v l = over v (\val -> f <$> ifNothing val (_splitRatio l)) l

incSideContainerWidth ::
     MiddleColumn l -> ModifySideContainerWidth -> MiddleColumn l
incSideContainerWidth l IncrementLeftColumnContainerWidth =
  colWidthOver widthInc leftContainerWidth l
incSideContainerWidth l IncrementRightColumnContainerWidth =
  colWidthOver widthInc rightContainerWidth l
incSideContainerWidth l DecrementLeftColumnContainerWidth =
  colWidthOver widthDec leftContainerWidth l
incSideContainerWidth l DecrementRightColumnContainerWidth =
  colWidthOver widthDec rightContainerWidth l
incSideContainerWidth l ResetColumnContainerWidth =
  (leftContainerWidth .~ Nothing) . (rightContainerWidth .~ Nothing) $ l

mainSplit :: MiddleColumn a -> Rectangle -> [Rectangle]
mainSplit z (Rectangle sx sy sw sh) = columnSwops z [m, l, r]
  where
    f = _splitRatio z
    splitWLeft = floor $ fromIntegral sw * (maybe f id (_leftContainerWidth z))
    splitWRight =
      floor $ fromIntegral sw * (maybe f id (_rightContainerWidth z))
    splitWMiddle = sw - (splitWLeft) - (splitWRight)
    l = Rectangle sx sy splitWLeft sh
    m = Rectangle (sx + fromIntegral splitWLeft) sy (splitWMiddle) sh
    r =
      Rectangle
        ((fromIntegral sw) - (fromIntegral splitWRight))
        sy
        splitWRight
        sh
