module Nicolo.Main where

import XMonad
import XMonad.Actions.DynamicProjects
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import Nicolo.Hooks.DynamicProjects
import Nicolo.Hooks.HandleEventHook
import Nicolo.Hooks.LayoutHook
import Nicolo.Hooks.LogHook
import Nicolo.Hooks.ManageHook
import Nicolo.Hooks.StartupHook
import Nicolo.Util.CustomVariables
import Nicolo.Util.Keys
import Nicolo.Util.Mouse

------------------------------------------------------------------------

main = xmonad
     $ dynamicProjects projects
     $ docks
     $ withUrgencyHook NoUrgencyHook
       defaults

{-|
  A structure containing your configuration settings, overriding
  fields in the default config. Any you don't override, will
  use the defaults defined in xmonad/XMonad/Config.hs

  No need to modify this.
-}
defaults = def
  { terminal           = myTerminal
  , focusFollowsMouse  = myFocusFollowsMouse
  , clickJustFocuses   = myClickJustFocuses
  , borderWidth        = myBorderWidth
  , modMask            = myModMask
  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor

  , keys               = myKeys
  , mouseBindings      = myMouseBindings

  , layoutHook         = myLayout
  , manageHook         = myManageHook
  , handleEventHook    = myHandleEventHook
  , logHook            = myLogHook
  , startupHook        = myStartupHook
  }

