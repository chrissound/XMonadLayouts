module WindowCoordinates where

import Data.List (sortBy)
import Data.Ord
import Graphics.X11.Xlib.Types

-- results xs = sortBy (compare `on` fst) (frequency xs)


topLeftSort :: [Rectangle] -> [Rectangle]
topLeftSort = sortBy (comparing rect_x <> comparing rect_y)

topRightSort :: [Rectangle] -> [Rectangle]
topRightSort = sortBy (comparing (negate . rect_x) <> comparing rect_y)

bottomLeftSort :: [Rectangle] -> [Rectangle]
bottomLeftSort = sortBy (comparing rect_x <> comparing (negate . rect_y))

bottomRightSort :: [Rectangle] -> [Rectangle]
bottomRightSort = sortBy (comparing (negate . rect_x) <> comparing (negate . rect_y))
