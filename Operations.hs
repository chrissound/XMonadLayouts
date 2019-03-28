{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Operations where

import Types
import Control.Lens
import XMonad


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

ifNothing :: Maybe a -> a -> Maybe a
ifNothing (Just x) _ = Just x
ifNothing (Nothing) v = Just v

widthInc, widthDec :: Float -> Float
widthInc = (+ widthDelta)

widthDec = flip (-) widthDelta

widthDelta :: Float
widthDelta = 0.02

modifyMiddleColumn :: MiddleColumnModify -> MiddleColumn a -> MiddleColumn a
modifyMiddleColumn _mcm@MiddleColumnModify{..} mc = 
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

columnSwops :: MiddleColumn a -> [Rectangle] -> [Rectangle]
columnSwops l (middleRec:leftRec:rightRec:[]) =
  case (_columnSwop l) of
    ResetColumn -> [middleRec, leftRec, rightRec]
    SwopLeftColumn -> [leftRec, middleRec, rightRec]
    SwopRightColumn -> [rightRec, leftRec, middleRec]
columnSwops _ r = r
