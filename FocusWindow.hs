{-# LANGUAGE BangPatterns #-}
module FocusWindow where

import XMonad.StackSet (Stack(Stack), focus, up, down)
import qualified XMonad.StackSet as W
import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)
import XMonad (windows, X, WindowSet, XState(XState, windowset))
import Control.Monad.State

traceTraceShowId :: Show a => String -> a -> a
traceTraceShowId x = traceShow x . traceShowId

getLast :: Int -> [a] -> a
getLast n = head . reverse . take n . reverse

focusWindow ::(Eq s, Eq a, Eq i) => Int -> W.StackSet i l a s sd -> W.StackSet i l a s sd
focusWindow n x = if (nFetch <= (windowLength))
       then W.focusWindow (W.index x !! nFetch) x
       else x
  where
    windowLength = length $ W.index x
    nFetch = if (n > 0)
          then n
          else getLastNthWindowIndex n windowLength

getLastNthWindowIndex :: Int -> Int -> Int
getLastNthWindowIndex n wl = if n < (wl) then
                              getLast n [0..wl -1]
                            else (wl - 1)

swopElementsAt :: Int -> Int -> [a] -> [a]
swopElementsAt f s xs = zipWith (\x y -> 
    if x == f then xs !! s
    else if x == s then xs !! f
    else y) [0..] xs

data WindowStackPosition = Top | Focus | Bottom deriving (Eq, Show)

swopStackElements :: Show a => Int -> Int -> Stack a -> Stack a
swopStackElements i j s@(W.Stack f t b) = do
  let w = W.integrate s
  let swopped = if ((i < length w) && (j < length w))
      then swopElementsAt i j $ w
      else w
  let z = zipWith (,) wsp swopped
          where
            wsp = (replicate (length t) Top) ++ [Focus] ++ (replicate (length b) Bottom)
  let nU = filter ((==) Top . fst) z
  let nB = filter ((==) Bottom . fst) z
  unsafePerformIO $ do 
    let !_ = traceTraceShowId "New line!" ""
    let !_ = traceTraceShowId "i" (i)
    let !_ = traceTraceShowId "j" (j)
    let !_ = traceTraceShowId "original stack" z
    let !_ = traceTraceShowId "swopped stack" swopped
    let !_ = traceTraceShowId "top length" (length t)
    let !_ = traceTraceShowId "bottom length" (length b)
    let !_ = traceTraceShowId "new top length" (length nU)
    let !_ = traceTraceShowId "new bottom length" (length nB)
    return $ Stack { focus = f, up = snd <$> nU, down = snd <$> nB }

swopWindow ::(Eq s, Eq a, Eq i) => Int -> Int -> W.StackSet i l a s sd -> W.StackSet i l a s sd
swopWindow i i' s | i == i' = s
                  | i > i' = swopWindow i' i s
                  | otherwise = s { W.visible = swopElementsAt i i' $ W.visible s}

swopWindowToMaster :: Int -> X ()
swopWindowToMaster n = do
  windows $ focusWindow n
  windows $ W.swapMaster

getWindowState :: X (WindowSet)
getWindowState = do
    XState { windowset = old } <- get
    return $ old
