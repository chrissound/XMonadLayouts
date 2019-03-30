{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WindowColumn where

import           XMonad
import WindowCoordinates
import Data.List

data SwopSideColumnWindow n = SwopLeft n | SwopRight n deriving (Show, Typeable)
instance Message (SwopSideColumnWindow Int)

data Column = Left | Middle | Right deriving Show

data WindowDirection = Up | Down deriving Show

data WindowPosition = WindowPosition { wIndex :: Int, wColumn ::  Column, wDirection ::  WindowDirection} deriving Show

data SwopTo = SwopTo
  {
    from :: WindowPosition
  , to :: WindowPosition
  }

  deriving (Show)
instance Message (SwopTo)

windowPositionToWindow :: WindowPosition -> [Rectangle] -> Rectangle
windowPositionToWindow wp r = case (wColumn wp, wDirection wp) of
  (WindowColumn.Left, Up) -> topLeftSort r        !! (wIndex wp - 1)
  (WindowColumn.Left, Down) -> bottomLeftSort r   !! (wIndex wp - 1)
  (WindowColumn.Right, Up) -> topRightSort r      !! (wIndex wp - 1)
  (WindowColumn.Right, Down) -> bottomRightSort r !! (wIndex wp - 1)
  _ -> error ""

windowPositionToStacksetIndex :: WindowPosition -> [Rectangle] -> Maybe Int
windowPositionToStacksetIndex wp r = elemIndex (windowPositionToWindow wp r) r
