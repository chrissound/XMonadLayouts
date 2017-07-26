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

swopWindowToMaster :: Int -> X ()
swopWindowToMaster n = do
  windows $ focusWindow n
  windows $ W.swapMaster
