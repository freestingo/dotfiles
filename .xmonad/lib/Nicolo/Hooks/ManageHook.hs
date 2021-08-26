module Nicolo.Hooks.ManageHook where

import           XMonad
import           XMonad.Actions.SpawnOn
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import           XMonad.Util.NamedScratchpad
import           Nicolo.Util.CustomVariables

{-|
    Window rules:

    Execute arbitrary actions and WindowSet manipulations when managing
    a new window. You can use this to, for example, always float a
    particular program, or have a client always appear on a particular
    workspace.

    To find the property name associated with a program, use
    > xprop | grep WM_CLASS
    and click on the client you're interested in.

    To match on the WM_NAME, you can use 'title' in the same way that
    'className' and 'resource' are used below.

    Check also https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips
-}
myManageHook = composeAll (concat
    [
      [ isDialog                --> doSideFloat C ]
    , [ className =? fc         --> doCenterFloat | fc <- myFloatClasses ]
    , [ className =? rfc        --> doRectFloat (W.RationalRect 0.15 0.15 0.7 0.7) | rfc <- myResizeFloatClasses ]
    , [ className =? rb         --> hasBorder False | rb <- myRemoveBorderClasses ]
    , [ resource  =? ir         --> doIgnore | ir <- myIgnoreResources ]
    , [ manageSpawn ]
    ])
    <+> namedScratchpadManageHook myScratchpads
      where myFloatClasses = ["Mplayer", "Gimp", "Java", "Pavucontrol", "Gnome-calculator", "Org.gnome.Nautilus"]
            myResizeFloatClasses = ["nessuna"]
            myRemoveBorderClasses = ["VirtualBox Machine", "org.gnome.Nautilus"]
            myIgnoreResources = ["desktop_window", "kdesktop"]
            --viewShift = doF . liftM2 (.) W.greedyView W.shift

