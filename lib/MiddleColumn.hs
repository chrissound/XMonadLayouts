{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wno-orphans #-}

module MiddleColumn (
    module MiddleColumn
  , module Types
  , middleColumnModifyId
                    ) where

import Control.Monad
import Control.Lens
import FocusWindow hiding (traceTraceShowId)
import XMonad
import qualified XMonad.StackSet as W
import XMonad.StackSet (modify')

import Data.Foldable
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe
import Debug.Trace
import WindowColumn
  -- ( Column(Left, Middle, Right)
  -- , SwopSideColumnWindow(..)
  -- , SwopTo(SwopTo)
  -- , wColumn
  -- , wIndex
  -- , wDirection
  -- , WindowDirection (..)
  -- , WindowPosition (..)
  -- )

import MyDebug
-- import WindowCoordinates
import WindowFinder
import Operations
import Types
-- import RearrangeWindows

traceTraceShowId :: Show a => String -> a -> a
traceTraceShowId x = traceShow x . traceShowId


masterColumnWindowCount :: MiddleColumn a -> Int
masterColumnWindowCount l = _middleColumnCount l +
  case _splitMasterWindow l of
        Nothing -> 0
        Just ToggleMasterColumnSplit -> 1
        Just ToggleMasterColumnSplitAll -> _middleColumnCount l

getMiddleColumnSaneDefault ::
     Int -> Float -> (Float, Float, Float) -> MiddleColumn a
getMiddleColumnSaneDefault mColumnCount mTwoRatio mThreeRatio =
  MiddleColumn
  { _splitRatio = 0.25 - 1 * 0.04
  , _splitMasterWindow = Nothing
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

-- Example: MiddleColumn 0.25 1 0.040 0.25

-- If zero then return no rectangles
splitVerticallyFixed :: Int -> Rectangle -> [Rectangle]
splitVerticallyFixed 0 _ = []
splitVerticallyFixed c r = splitVertically c r

xAccumulateRecatangle :: [Rectangle] -> [Rectangle]
xAccumulateRecatangle [] = []
xAccumulateRecatangle [r1] = [r1]
xAccumulateRecatangle [r1, r2] =
  r1 :
  [ r2
    { rect_x =
        floor $
        fromIntegral (rect_x r1) + (fromIntegral $ rect_width r1 :: Float)
    }
  ]
xAccumulateRecatangle (r1:r2:r3) = do
  let [ar1, ar2] = xAccumulateRecatangle (r1 : [r2])
  ar1 : xAccumulateRecatangle (ar2 : r3)

splitHorizontallyByRatios :: [Float] -> Rectangle -> [Rectangle]
splitHorizontallyByRatios ratios mainR@(Rectangle _ _ w _) = do
  let widthSet =
        fmap
          (\ratio -> mainR {rect_width = floor $ fromIntegral w * ratio})
          ratios
  xAccumulateRecatangle widthSet

splitVerticallyByRatios :: [Float] -> Rectangle -> [Rectangle]
splitVerticallyByRatios f =
  fmap mirrorRect . splitHorizontallyByRatios f . mirrorRect

getRecsWithSideContainment ::
     Rectangle -> Rectangle -> Int -> Int -> Int -> ([Rectangle], [Rectangle])
-- Show window on entire left rec if a single window is needed, and there is no 'pinning'
getRecsWithSideContainment lRec _ 0 0 1 = ([lRec], [])
-- divide equally between left and right
getRecsWithSideContainment lRec rRec 0 0 totalCount =
  (splitVerticallyFixed lCount lRec, reverse (splitVerticallyFixed rCount rRec))
  where
    (lCount, rCount) = splitDiscrete totalCount
    splitDiscrete a = (b, a - b)
      where
        b = quot a 2
-- divide with a max count on left or right
getRecsWithSideContainment lRec rRec leftMax rightMax totalCount =
  (\(i, j) -> (i, reverse j)) $
  if leftMax > 0
    then ( splitVerticallyFixed leftMax lRec
         , splitVerticallyFixed (totalCount - leftMax) rRec)
    else ( splitVerticallyFixed (totalCount - rightMax) lRec
         , splitVerticallyFixed rightMax rRec)



layoutRectangles :: Show a => MiddleColumn a1 -> Rectangle -> W.Stack a -> [(a, Rectangle)]
layoutRectangles l screenRec s = zip ws (a++b++c) where
      ws = W.integrate (mdid' MyDebugXmonadWin "the stack" s)
      (a,b,c) = layoutRectangles' l screenRec $ length ws

layoutRectangles' :: MiddleColumn a1 -> Rectangle -> Int -> ([Rectangle],[Rectangle],[Rectangle])
layoutRectangles' l screenRec s = mdid' MyDebugRecs "recs" (middleRecs, leftInnerRecs, rightInnerRecs)
    where
      mcc = _middleColumnCount l
      mctRatio = _middleTwoRatio l
      mc3Ratio = _middleThreeRatio l
      [middleRec,leftRec,rightRec] = mainSplit l screenRec
      sortByHeightDesc =  sortBy (flip compare `on` rect_height)
      middleRecs =
        case _splitMasterWindow l of
          Nothing -> id
          Just ToggleMasterColumnSplit -> \(r:rx) -> splitHorizontally 2 r ++ rx
          Just ToggleMasterColumnSplitAll -> (splitHorizontally 2 =<<)
        $
        -- If there are two windows in the "middle column", make the larger window the master
        if mcc == 2
          then sortByHeightDesc $
               (\(m1, m2) -> [m1, m2]) $ splitVerticallyBy mctRatio middleRec
          else if mcc == 3
                 then sortByHeightDesc $
                      splitVerticallyByRatios
                        ((\(m1, m2, m3) -> [m1, m2, m3]) mc3Ratio)
                        middleRec
                 else splitVertically mcc middleRec
      (leftInnerRecs, rightInnerRecs) =
        mdid' MyDebugRecs "recs" $
        getRecsWithSideContainment
          leftRec
          rightRec
          (_leftContainerCount l)
          (_rightContainerCount l)
          (mdid' MyDebugRecs "sssss" s - mdid' MyDebugRecs "..." (masterColumnWindowCount l))

getWindowCount :: X Int
getWindowCount = length . W.integrate' . W.stack . W.workspace . W.current . windowset <$> get

getScreenRes :: X Rectangle
getScreenRes = screenRect . W.screenDetail . W.current . windowset <$> get

instance (Show a) => LayoutClass MiddleColumn a where

  description _ = "MiddleColumn"
  doLayout l r s = do
    let mcc = masterColumnWindowCount l
    let lContainerCount = _leftContainerCount l
    let rContainerCount = _rightContainerCount l
    let sideColumnWindowCount = mdid' MyDebugXmonadWin "sideColumnWindowCount" (length $ W.integrate s) - mcc
    -- let yolo | True = 123
    --          | False = fuck
    --            { _rightContainerCount = oeu}
    let l' | lContainerCount > 0 = l {_leftContainerCount = lcc, _rightContainerCount = -lcc}
           | rContainerCount > 0 = l
                        { _leftContainerCount = -rcc
                        , _rightContainerCount = rcc
                        }
           | otherwise = l
           where
           lcc = min sideColumnWindowCount lContainerCount
           rcc = min sideColumnWindowCount rContainerCount
    return (pureLayout l' r s, Just l')
  pureLayout = layoutRectangles
  pureMessage l m =
    msum
      [ fmap resize (fromMessage m)
      , fmap incmastern (fromMessage m)
      , fmap (`incSideContainer` l) (fromMessage m)
      , fmap (incSideContainerWidth l) (fromMessage m)
      , fmap (\x -> l & columnSwop .~ x) (fromMessage m)
      , fmap (`modifyMiddleColumn` l) (fromMessage m)
      ]
    where
      sRatio = _splitRatio l
      -- column swops
      resize Expand = l & splitRatio .~ min 0.5 (sRatio + 0.04)
      resize Shrink = l & splitRatio .~ max 0 (sRatio - 0.04)
      incmastern (IncMasterN x) = l & middleColumnCount .~ max 0 (l ^. middleColumnCount + x)
  handleMessage l m = do
    ws <- getWindowState >>= (return . W.stack . W.workspace . W.current)
    let possibleMessages =
          [
            case (fromMessage m :: Maybe (FocusWindow' WindowPosition)) of
              (Just (FocusWindow' wp)) ->
                return $ do
                  sr <- getScreenRes
                  case ws of
                    Just ws' -> do
                      let r = snd <$> layoutRectangles l sr ws'
                      case windowPositionToStacksetIndex wp r of
                        Just i -> do
                          windows $ focusWindow $ traceTraceShowId "FocusWindow:" i
                          return Nothing
                        Nothing -> error "???"
                    Nothing -> pure Nothing
              _ -> Nothing
            ,case fromMessage m :: Maybe SwopWindow' of
              Just (SwopWindow' wp) -> return $ do
                  sr <- getScreenRes
                  ws''' <- withWindowSet pure
                  case ws of
                    Just ws' -> do
                      let r = snd <$> layoutRectangles l sr ws'
                      case windowPositionToStacksetIndex wp r of
                        Just i -> 
                            pure (W.peek ws''' >>= flip windowIndex ws''') >>= \case
                              Just (currentWindowIndex' :: Int) -> do
                                Debug.Trace.trace "swopstack..." windows . W.modify' $ swopStackElements i currentWindowIndex'
                                return Nothing
                              _ -> pure Nothing
                        Nothing -> pure Nothing
                    Nothing -> pure Nothing
              _ -> Nothing
          , case fromMessage m :: Maybe SwopTo' of
              Just (SwopTo' f t) -> return $ do
                sr <- getScreenRes
                case ws of
                  Just ws' -> do
                    let w = fst <$> layoutRectangles l sr ws'
                    let myRecs = layoutRectangles' l (Rectangle 1000 1000 1000 1000) (length ws)
                    let f' = normalizeSwopWindowPosition f w myRecs
                    let t' = normalizeSwopWindowPosition t w myRecs
                    case (f', t') of
                      (Just f'', Just t'') -> do
                        windows $ modify' $ swopStackElements f'' t''
                        pure Nothing
                      _ -> error "Unresolvable SwopTo param"
                  _ -> pure Nothing
              _ -> Nothing
          , case (fromMessage m :: Maybe ToggleMasterColumnSplit) of
              x'''@(Just x) -> pure . pure . pure $ (l & splitMasterWindow .~ xx) where
                  xx = case _splitMasterWindow l of
                    Nothing -> Just x
                    _ ->
                      if _splitMasterWindow l == x''' then
                        Nothing
                      else
                        Just x
              _ -> Nothing
          ]
    case asum possibleMessages of
      Just x -> x
      _ -> pure $ pureMessage l m

getWindowIndex :: WindowPosition -> Int -> Int -> Int -> Int -> Int
getWindowIndex w leftWindowOffset leftWindowCount rightWindowCount windowCount =
  case wColumn w of
    Types.Left -> case wDirection w of
      Up ->   leftWindowOffset + wIndex w
      Down -> leftWindowOffset - wIndex w + leftWindowCount
    Types.Right -> case wDirection w of
      Up -> getLastNthWindowIndex (wIndex w) windowCount
      Down -> getLastNthWindowIndex (wIndex w - rightWindowCount) windowCount
    Types.Middle -> wIndex w


mainSplit :: MiddleColumn a -> Rectangle -> [Rectangle]
mainSplit z (Rectangle sx sy sw sh) = columnSwops z [m, l, r]
  where
    f = _splitRatio z
    splitWLeft = floor $ fromIntegral sw * fromMaybe f (_leftContainerWidth z)
    splitWRight =
      floor $ fromIntegral sw * fromMaybe f (_rightContainerWidth z)
    splitWMiddle = sw - splitWLeft - splitWRight
    l = Rectangle sx sy splitWLeft sh
    m = Rectangle (sx + fromIntegral splitWLeft) sy splitWMiddle sh
    r =
      Rectangle
        (fromIntegral sw - fromIntegral splitWRight)
        sy
        splitWRight
        sh
