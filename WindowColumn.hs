{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WindowColumn where

import           XMonad

data SwopSideColumnWindow n = SwopLeft n | SwopRight n deriving Typeable
instance Message (SwopSideColumnWindow Int)


data Column = Left | Middle | Right

data SwopTo = SwopTo
  {
    from :: Int
  , to :: Int
  , column :: WindowColumn.Column
  }
instance Message (SwopTo)
