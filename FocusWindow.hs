{-# LANGUAGE BangPatterns #-}
module FocusWindow where

import XMonad (windows, X)
import XMonad.StackSet (Stack(Stack), focus, up, down)
import qualified XMonad.StackSet as W
import Debug.Trace
import System.IO.Unsafe

traceTraceShowId :: Show a => String -> a -> a
traceTraceShowId x = traceShow x . traceShowId

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

data WindowStackPosition = Top | Focus | Bottom deriving (Eq)

swopStackElements :: Int -> Int -> Stack a -> Stack a
swopStackElements i j s@(W.Stack f t b) = do
 let z = zipWith (,) wsp (W.integrate s)
         where
           wsp = (replicate (length t) Top) ++ [Focus] ++ (replicate (length b) Bottom)
 let swopped = swopElementsAt i j $ z
 let nU = filter ((==) Top . fst) swopped
 let nB = filter ((==) Bottom . fst) swopped
 unsafePerformIO $ do 
  let !_ = traceTraceShowId "New line!" ""
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
