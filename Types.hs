{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Types where

import XMonad
import Text.Read
import Control.Lens

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

data SwopSideColumn
  = SwopLeftColumn
  | SwopRightColumn
  | ResetColumn
  deriving (Show, Typeable)

data SwopSideColumnWindow n = SwopLeft n | SwopRight n deriving (Show, Typeable)
instance Message (SwopSideColumnWindow Int)

data Column = Left | Middle | Right deriving Show

instance Read SwopSideColumn where
  readPrec = return (ResetColumn)
  readListPrec = readListPrecDefault


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

data WindowDirection = Up | Down deriving Show

data WindowPosition = WindowPosition { wIndex :: Int, wColumn ::  Column, wDirection ::  WindowDirection} deriving Show


newtype FocusWindow' a = FocusWindow' a
instance Message (FocusWindow' WindowPosition)
data SwopWindow' = SwopWindow' WindowPosition | SwopWindowIndex Int | SwopWindowWindow Window deriving Show
  -- | SwopWindowWindow Window deriving Show
instance Message (SwopWindow')


instance Message (SwopSideColumn)

data ModifyLayout =
  ModifyLayout (MiddleColumnModify)

instance Message (MiddleColumnModify)

data ToggleMasterColumnSplit = ToggleMasterColumnSplit | ToggleMasterColumnSplitAll deriving (Read,Show, Eq)
instance Message (ToggleMasterColumnSplit)

data RearrangeWindows = RearrangeWindows deriving (Read,Show, Eq)
instance Message (RearrangeWindows)

data MiddleColumn a = MiddleColumn
  { _splitRatio :: Float -- width ratio of side columns
  , _splitMasterWindow :: Maybe (ToggleMasterColumnSplit)
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


data MiddleColumnEnum
  = LColumn
  | MColumn
  | RColumn

data SwopTo' = SwopTo'
  {
    from' :: SwopWindow'
  , to' :: SwopWindow'
  }

  deriving (Show)
instance Message (SwopTo')



-- data SwopTo = SwopTo
--   {
--     from :: WindowPosition
--   , to :: WindowPosition
--   }

--   deriving (Show)
-- instance Message (SwopTo)

type MiddleColumnRecs = ([Rectangle],[Rectangle],[Rectangle])
