module Nicolo.Util.Prompt where

import XMonad
import XMonad.Prompt

switchWorkspacePrompt :: XPConfig
switchWorkspacePrompt = styledPrompt "Switch to workspace: "

shiftToWorkspacePrompt :: XPConfig
shiftToWorkspacePrompt = styledPrompt "Move to workspace: "

deleteWorkspacePrompt :: XPConfig
deleteWorkspacePrompt = styledPrompt "Remove workspace: "

styledPrompt :: String -> XPConfig
styledPrompt promptMsg = def { font = "xft:Hasklug Nerd Font Mono:weight=bold:pixelsize=17:antialias=true:hinting=true"
                             , bgColor = "#1B1C22"
                             , borderColor = "#CDCFD4"
                             , height = 40
                             , promptBorderWidth = 3
                             , position = CenteredAt { xpCenterY = 0.43, xpWidth = 0.4 }
                             , defaultPrompter = const promptMsg
                             , maxComplRows = Just 6
                             }

