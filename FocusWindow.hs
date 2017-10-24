module FocusWindow where

import qualified XMonad.StackSet as W
import XMonad

getLast :: Int -> [a] -> a
getLast n = head . reverse . take n . reverse

focusWindow ::(Eq s, Eq a, Eq i) => Int -> W.StackSet i l a s sd -> W.StackSet i l a s sd
focusWindow n x = if (nFetch <= (windowLength))
       then W.focusWindow (W.index x !! nFetch) x
       else id x
  where
    windowLength = length $ W.index x
    nFetch = if (n > 0)
          then n
          else getLast (negate n) [0..(windowLength -1)]


swopElementsAt :: Int -> Int -> [a] -> [a]
swopElementsAt i j xs = (take i xs) ++ [xs !! j] ++ middle ++ [xs !! i] ++ right where
                        middle = take (j - i - 1) (drop (i + 1) xs)
                        right = drop (j + 1) xs

swopWindow ::(Eq s, Eq a, Eq i) => Int -> Int -> W.StackSet i l a s sd -> W.StackSet i l a s sd
swopWindow i i' s | i == i' = s
                  | i > i' = swopWindow i' i s
                  | otherwise = s { W.visible = swopElementsAt i i' $ W.visible s}


swopWindowToMaster :: Int -> X ()
swopWindowToMaster n = do
  windows $ focusWindow n
  windows $ W.swapMaster
