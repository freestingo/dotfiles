module Nicolo.Hooks.LayoutHook where

import           XMonad
import           XMonad.Hooks.ScreenCorners
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.Drawer
import           Nicolo.Layout.LimitWindows -- with custom `increaseLimit` and layout description modifier
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.TwoPane
import           XMonad.Hooks.ManageDocks

{-|
    Layouts:

    You can specify and transform your layouts by modifying these values.
    If you change layout bindings be sure to use 'mod-shift-space' after
    restarting (with 'mod-q') to reset your layout state to the new
    defaults, as xmonad preserves your old layout settings by default.

    The available layouts.  Note that each layout is separated by |||,
    which denotes layout choice.
-}

myLayout = screenCornerLayoutHook
         -- First padding value is screen-related, second one is windows-related
         -- https://www.reddit.com/r/xmonad/comments/n05z0o/questions_about_gaps_and_multimonitor/
         $ renamed [CutWordsLeft 1] $ spacingRaw False (Border 0 50 0 50) True (Border 50 0 50 0) True
         $ avoidStruts
         $ BW.boringAuto
         $ (onWorkspace "vm" . BW.boringAuto . renamed [Replace "VM Layout"] . noBorders $ minimize Simplest)
         (   renamed [Replace "Simplest"] (minimize Simplest)
         ||| tiled
         ||| Mirror tiled
         -- ||| drawer `onLeft` tiled
         )
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled = renamed [CutWordsLeft 1] . minimize . limitWindows 2 $ ResizableTall nmaster delta ratio slaves
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio = 1/2
     -- Percent of screen to increment by when resizing panes
     delta = 3/100
     -- fraction to multiply the window height that would be given when divided equally.
     -- slave windows are assigned their modified heights in order, from top to bottom
     -- unspecified values are replaced by 1
     slaves = []
     -- cool drawer layout
     drawer = simpleDrawer (1/100) (1/4) (ClassName "Firefox")

