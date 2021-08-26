module Nicolo.Actions.Custom where

import           Control.Monad
import           Data.Foldable
import           XMonad
import           XMonad.Actions.NoBorders
import           XMonad.Actions.TagWindows
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet as W

import           Nicolo.Util.Functions

{-|
   Execute an X action to all windows in the current workspace.
   Inspired by:
      https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-WithAll.html
      https://stackoverflow.com/questions/47051557/how-can-i-get-the-count-of-windows-in-the-current-workspace
-}
withAllWindows :: (Window -> X ()) -> X ()
withAllWindows action = traverse_ action . W.index . windowset =<< get

{-|
   Send notification with `notify-send`.
   Title and body can be formatted with some basic HTML.
-}
sendNotification :: MonadIO m => String -> String -> m ()
sendNotification title body = spawn $ "notify-send -u low " ++ safeArgs [title, body]

{-|
   Shift window to the given workspace id and send a notification
   only if said workspace is not visible.
-}
shiftToAndNotify :: WorkspaceId -> ManageHook
shiftToAndNotify ws = do
                        cw <- currentWs
                        unless (cw == ws) $ sendNotification title body
                        doShift ws
                  where title = "XMonad LayoutHook"
                        body = "Moved window to <b>" ++ ws ++ "</b> workspace!"

modifyWindowPadding :: Integer -> Border -> Border
modifyWindowPadding x (Border t b l r) = Border (t + x) b (l + x) r

modifyScreenPadding :: Integer -> Border -> Border
modifyScreenPadding x (Border t b l r) = Border t (b + x) l (r + x)

{-|
   Modify current padding value by `amount`.
   Both window and screen padding must be changed at the same time in order to
   keep the spacing uniform when opening multiple windows on the same screen.
-}
modifyPadding :: Integer -> X ()
modifyPadding amount = do
    sendMessage $ ModifyWindowBorder (modifyWindowPadding amount)
    sendMessage $ ModifyScreenBorder (modifyScreenPadding amount)

{-|
   Toggles actual full-screen mode.
   In full-screen mode, other than disabling spacings and borders,
   all open windows in the current workspace will be tagged in order to
   make them recognizable by `picom`, so that it can disable rounded corners.
-}
toggleFullScreen :: X ()
toggleFullScreen = do
    withAllWindows $ toggleBorder <> toggleTag
    toggleScreenSpacingEnabled
    toggleWindowSpacingEnabled
    sendMessage ToggleStruts
    where toggleTag win = do
            hasNoRoundCorners <- hasTag "no-round-corners" win
            if hasNoRoundCorners
              then delTag "no-round-corners" win
              else addTag "no-round-corners" win

