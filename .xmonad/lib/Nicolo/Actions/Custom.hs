module Nicolo.Actions.Custom where

import           Control.Monad
import           Data.Char
import           Data.Foldable
import           System.Process
import           XMonad
import           XMonad.Actions.NoBorders
import           XMonad.Actions.TagWindows
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Gaps
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import           XMonad.Util.Run

import           Nicolo.Util.Functions
import           Nicolo.Util.CustomVariables
import           Nicolo.Layout.MouseResizableTile

{-|
  Execute an X action to all windows in the current workspace.
  Inspired by:
    https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-WithAll.html
    https://stackoverflow.com/questions/47051557/how-can-i-get-the-count-of-windows-in-the-current-workspace
-}
withAllWindows :: (Window -> X ()) -> X ()
withAllWindows action = traverse_ action . W.index . windowset =<< get

sendNotification :: MonadIO m => String -> String -> String -> m ()
sendNotification urgency title body = spawn $ "notify-send -u " ++ urgency ++ " " ++ safeArgs [title, body]

sendTaggedNotification :: MonadIO m => String -> String -> String -> String -> m ()
sendTaggedNotification urgency tag title body = spawn $ "dunstify -u " ++ urgency ++ " -h string:x-dunst-stack-tag:" ++ tag ++ " " ++ safeArgs [title, body]

{-|
  Send low-urgency notification with `notify-send`.
  Title and body can be formatted with some basic HTML.
-}
sendLowNotification :: MonadIO m => String -> String -> m ()
sendLowNotification = sendNotification "low"

sendNormalNotification :: MonadIO m => String -> String -> m ()
sendNormalNotification = sendNotification "normal"

{-|
  Send low-urgency tagged notification with `dunstify`;
  this way, newer notifications will replace older ones if they
  share the same tag (useful for brighness/volume changes, etc.)
-}
sendLowTaggedNotification :: MonadIO m => String -> String -> String -> m ()
sendLowTaggedNotification = sendTaggedNotification "low"

{-|
  Shift window to the given workspace id and send a notification
  only if said workspace is not visible.
-}
shiftToAndNotify :: WorkspaceId -> ManageHook
shiftToAndNotify ws = do
  cw <- currentWs
  unless (cw == ws) $ sendLowNotification title body
  doShift $ if ws `elem` myWorkspaces then ws else cw
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
  Like `modifyPadding`, but meant for mouse-resizable layouts.
-}
modifyPadding' :: Int -> X ()
modifyPadding' amount = do
  traverse_ (sendMessage . IncGap amount) [U, D, R, L]
  sendMessage $ ModifyDraggerGap (fromIntegral amount)

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

{-|
  Like `toggleFullScreen`, but with added support for mouse-resizable layouts.
-}
toggleFullScreen' :: X ()
toggleFullScreen' = do
  withAllWindows $ toggleBorder <> toggleTag
  sendMessage ToggleDraggerGap
  sendMessage ToggleGaps
  sendMessage ToggleStruts
  where toggleTag win = do
        hasNoRoundCorners <- hasTag "no-round-corners" win
        if hasNoRoundCorners
          then delTag "no-round-corners" win
          else addTag "no-round-corners" win

data BrightnessAction = IncreaseBrightness | DecreaseBrightness

instance Show BrightnessAction where
  show IncreaseBrightness = " -a "
  show DecreaseBrightness = " -s "

type PercentAmount = Int

modifyBrightness :: BrightnessAction -> PercentAmount -> X ()
modifyBrightness action amount = do
  spawn $ "lux" ++ show action ++ show amount ++ "%"
  -- TODO find out why this actually shows the level before the `spawn` on the line before was executed
  currentBrightness <- runProcessWithInput "lux" ["-G"] ""
  sendLowTaggedNotification "brightnessLvl" "XMonad Action" $ "Set brightness to <b>" ++ filter isPrint currentBrightness ++ "</b>!"

