module Nicolo.Hooks.DynamicProjects where

import Control.Monad
import XMonad
import XMonad.Actions.DynamicProjects
import XMonad.Hooks.ManageHelpers
import Nicolo.Util.Functions

{-|
    DYNAMIC PROJECTS!
    https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-DynamicProjects.html
-}

projects :: [Project]
projects =
    [ Project { projectName = "web"
              , projectDirectory = "~/Downloads/"
              , projectStartHook = Nothing
              }
    , Project { projectName = "oncode"
              , projectDirectory = "~/Documents/oncode/"
              , projectStartHook = Nothing
              }
    , Project { projectName = "npo"
              , projectDirectory = "~/Documents/oncode/projects/npo/"
              , projectStartHook = Nothing
              }
    , Project { projectName = "chat"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }
    , Project { projectName = "vm"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }
    ]

startChat :: X ()
startChat = spawn "firefox -new-window https://web.whatsapp.com/"

startSkype :: X ()
startSkype = spawn "skypeforlinux"

startVirtualBox :: X ()
startVirtualBox = spawn "virtualbox"

startOncode :: X ()
startOncode = do
                spawn "chromium --new-window teams.microsoft.com"
                spawn "chromium --new-window outlook.office.com/mail/inbox"
                spawn "chromium --new-window \"https://gitlab.com/oncodeit/oeds/fmc\""

startNpo :: X ()
startNpo = do
             spawn "teams"
             spawn "firefox -new-window outlook.office.com/mail/inbox"

myProjectStartHooks :: [(String, X ())]
myProjectStartHooks = [("oncode", startOncode), ("npo", startNpo), ("chat", startChat), ("skype", startSkype), ("vm", startVirtualBox)]

