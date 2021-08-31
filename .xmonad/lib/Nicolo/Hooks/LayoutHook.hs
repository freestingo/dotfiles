module Nicolo.Hooks.LayoutHook where

import           XMonad
import           XMonad.Hooks.ScreenCorners
import qualified XMonad.Layout.BoringWindows as BW
import           XMonad.Layout.Drawer
import           XMonad.Layout.Gaps
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.TwoPane
import           XMonad.Hooks.ManageDocks

import           Nicolo.Layout.MouseResizableTile
import           Nicolo.Layout.LimitWindows -- with custom `increaseLimit` and layout description modifier

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
         $ avoidStruts
         $ gaps [(U, padding), (D, padding), (L, padding), (R, padding)]
         $ BW.boringAuto
         $ (onWorkspace "vm" . BW.boringAuto . renamed [Replace "VM Layout"] . noBorders $ minimize Simplest)
         (   renamed [Replace "Simplest"] (minimize Simplest)
         ||| (tiledModifiers $ mouseResizableTile { draggerType = dragger })
         ||| (mirror . tiledModifiers $ mouseResizableTileMirrored { draggerType = dragger })
         -- ||| drawer `onLeft` tiled
         )
  where
     tiledModifiers = renamed [CutWordsLeft 1] . minimize . limitWindows 2
     mirror = renamed [PrependWords "Mirror"]
     -- amount of padding pixels around windows
     padding :: Num a => a
     padding = 50
     -- default dragger
     dragger = FixedDragger padding padding
     -- default tiling algorithm partitions the screen into two panes
     tiled = tiledModifiers $ ResizableTall nmaster delta ratio slaves
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

