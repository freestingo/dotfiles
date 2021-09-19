module Nicolo.Hooks.LogHook where

import qualified Data.Map as M
import           Data.Maybe
import           XMonad
import           XMonad.Hooks.DynamicBars
import           XMonad.Hooks.DynamicLog

import           Nicolo.Util.CustomVariables
import           Nicolo.Util.Functions

{-|
  Status bars and logging

  Perform an arbitrary action on each internal state change or X event.
  See the 'XMonad.Hooks.DynamicLog' extension for examples.
-}
myLogHook = multiPP focusPP unfocusPP
  where focusPP = xmobarPP
          { ppCurrent = xmobarColor "#F79EED" "" . wrap "[" "]"         -- current workspace
          , ppVisible = xmobarColor "#F2B3C9" "" . clickable            -- visible but not current workspace
          , ppHidden = xmobarColor "#B392F0" "" . clickable             -- hidden workspaces with windows
          , ppHiddenNoWindows = xmobarColor "#9ECBFF" "" . clickable    -- hidden workspaces with no windows
          , ppTitle = xmobarColor "#B3AFC2" "" . shorten 70             -- title of active window
          , ppSep = "  "                                                -- separators
          , ppUrgent = xmobarColor "#F97583" "" . wrap "!" "!"          -- urgent workspace
          }
        unfocusPP = focusPP

clickable :: String -> String
clickable ws = "<action=xdotool key super+" ++ show index ++ ">" ++ ws ++ "</action>"
  where index = fromJust $ M.lookup ws workspaceIndexes
        workspaceIndexes = M.fromList $ zip myWorkspaces [1..]
