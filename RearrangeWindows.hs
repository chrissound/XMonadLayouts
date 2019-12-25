module RearrangeWindows where

import XMonad
import WindowFinder
import Types

rearrangeWindows'' :: X ()
rearrangeWindows'' = do
  findWindowsByTitle "ranger" >>= \case
    (x:_) -> sendMessage
      $
      SwopTo'
        (SwopWindowWindow x)
        (SwopWindow' $ WindowPosition { wIndex = 1, wColumn = Types.Left, wDirection = Up}) -- rearrangeWindows' (swopStackElements 0) x
    _ -> pure ()
  findWindowsInCurrentWorkspaceByTitlePrefix "magit">>= \case
    (x:_) -> sendMessage
      $
      SwopTo'
        (SwopWindowWindow x)
        (SwopWindow' $ WindowPosition { wIndex = 2, wColumn = Types.Left, wDirection = Up}) -- rearrangeWindows' (swopStackElements 0) x
    _ -> pure ()
  withDisplay (\d -> 
    withFocused (\w -> do
                    ((/= "Emacs") . resClass) <$> (liftIO $ getClassHint d w) >>= \case
                     True -> findEmacs
                     False -> pure ()
      )
    )
  where
    findEmacs = do
      findWindowsByClass "Emacs" >>= \case
        (x:_) -> sendMessage
          $
          SwopTo'
            (SwopWindowWindow x)
            (SwopWindow' $ WindowPosition { wIndex = 1, wColumn = Types.Middle, wDirection = Up}) -- rearrangeWindows' (swopStackElements 0) x
        _ -> pure ()
