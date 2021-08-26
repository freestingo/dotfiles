module Nicolo.Util.CustomVariables where

import           Data.Char
import           Data.Foldable
import qualified Data.Map as M
import           Data.Maybe
import           Control.Applicative (liftA2)
import           Control.Monad
import           System.Exit
import           XMonad
import           XMonad.Actions.GridSelect
import           XMonad.Actions.DynamicProjects
import           XMonad.Hooks.ManageHelpers
import           XMonad.Prompt
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Dmenu as D
import qualified XMonad.Util.Dzen as DZ
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

-- could be overridden by XMonad.Layout.NoBorders
myBorderWidth :: Dimension
myBorderWidth = 4

-- mod1Mask = left alt
-- mod4Mask = windows key
myModMask :: KeyMask
myModMask = mod1Mask

myWorkspaces :: [String]
myWorkspaces = [ "code"
               , "front"
               , "back"
               , "web"
               , "oncode"
               , "npo"
               , "chat"
               , "vm"
               , "fun"
               ]

myNormalBorderColor :: String
myNormalBorderColor  = "#1E2428"

myFocusedBorderColor :: String
myFocusedBorderColor = "#8494B8"

myGridSelectConfig :: HasColorizer a => GSConfig a
myGridSelectConfig = def { gs_font = "xft:Hasklug Nerd Font Mono:pixelsize=16:antialias=true:hinting=true"
                         , gs_cellheight = 30
                         , gs_cellwidth = 100
                         }

myScratchpads :: [NamedScratchpad]
myScratchpads = [ NS "terminal" spawnTerminal findTerminal manageTerminal
                , NS "todo" spawnTodo findTodo manageTodo
                ]
                  where spawnTerminal = "alacritty --title=Scratchpad"
                        findTerminal = title =? "Scratchpad"
                        manageTerminal = doRectFloat (W.RationalRect 0.15 0.15 0.7 0.7)

                        spawnTodo = "alacritty --title=TODO --command vim ~/Documents/oncode/projects/npo/todo"
                        findTodo = title =? "TODO"
                        manageTodo = doRectFloat (W.RationalRect 0.15 0.15 0.7 0.7)

myEmails :: M.Map String [(KeyMask, Char)]
myEmails = M.fromList
         . zip ["personal", "oncode", "npo"]
         . (fmap . fmap) toKeyBind
         $ ["n.traini1@yahoo.it", "nicolo.traini@oncode.it", "nicolo.traini@nposervices.com"]
           where toKeyBind c | c == '@'  = (mod5Mask, 'ò')
                             | otherwise = (noModMask, c)

