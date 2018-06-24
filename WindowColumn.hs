{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WindowColumn where

import           XMonad

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
