module WindowCoordinates where

import Data.List (sortBy)
import Data.Ord
import Graphics.X11.Xlib.Types

-- results xs = sortBy (compare `on` fst) (frequency xs)


topLeftRec :: [Rectangle] -> Rectangle
topLeftRec (x:[]) = x
topLeftRec r = head $ sortBy (comparing rect_x <> comparing rect_y) r


