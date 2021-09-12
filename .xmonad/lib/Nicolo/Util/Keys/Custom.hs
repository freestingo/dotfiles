module Nicolo.Util.Keys.Custom where

import           Data.Char
import qualified Data.Map as M
import           Data.Maybe
import           Data.Tuple (swap)
import           Control.Applicative (liftA2)
import           XMonad
import qualified XMonad.StackSet as W
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.GridSelect
import           XMonad.Actions.GroupNavigation
import           XMonad.Actions.Minimize
import           XMonad.Actions.NoBorders
import           XMonad.Actions.PerWorkspaceKeys
import           XMonad.Actions.RotSlaves
import           XMonad.Actions.TagWindows
import           XMonad.Actions.Warp
import           XMonad.Actions.WindowBringer
import           XMonad.Hooks.ManageDocks
import qualified XMonad.Layout.BoringWindows as BW
import           Nicolo.Layout.LimitWindows
import           XMonad.Layout.ResizableTile
import           Nicolo.Layout.MouseResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import qualified XMonad.Util.Paste as P

import           Nicolo.Actions.Custom
import qualified Nicolo.Actions.CycleWS as MyCWS
import           Nicolo.Hooks.DynamicProjects
import           Nicolo.Util.CustomVariables
import           Nicolo.Util.Functions
import           Nicolo.Util.Dmenu
import           Nicolo.Util.Prompt
import           Nicolo.Util.Scrot

myKeysWithDescription :: XConfig Layout -> [((String, String), X ())]
myKeysWithDescription conf =
  [ (("Launch a terminal", "M-C-<Return>"), spawn $ XMonad.terminal conf)
  , (("Launch dmenu", "M-p"), spawn $ unwords ["dmenu_run", "-c -l 10 -h 30", "-fn " ++ safeArg myDmenuFont, "-hp " ++ myDmenuFavorites, "-H ~/Documents/suckless/dmenu-5.0/histfile"])
  , (("Close focused window", "M-c"), kill)
  , (("Close all windows in current workspace", "M-S-c"), withAllWindows killWindow)
  , (("Move focus to the next non-boring window", "M-<Tab>"), BW.focusDown)
  , (("Move focus to the previous non-boring window", "M-S-<Tab>"), BW.focusUp)
  , (("Swap the focused window and the master window", "M-<Return>"), windows W.swapMaster)
  , (("Push window back into tiling", "M-t"), withFocused $ windows . W.sink)
  , (("Increment the number of windows in the master area", "M-,"), sendMessage (IncMasterN 1))
  , (("Deincrement the number of windows in the master area", "M-."), sendMessage (IncMasterN (-1)))
  , (("Swap current screen with next screen", "M-S-C-k"), swapNextScreen)
  , (("Swap current screen with previous screen", "M-S-C-j"), swapPrevScreen)
  , (("Quit xmonad (logout)", "M-S-q"), confirmLogout)
  , (("Restart xmonad", "M-q"), spawn "xmonad --recompile; xmonad --restart")
  , (("Go to next workspace", "M-l"), MyCWS.nextWS)
  , (("Go to previous workspace", "M-h"), MyCWS.prevWS)
  , (("Rotate through the available layout algorithms", "M-<Space>"), sendMessage NextLayout)
  , (("Reset the layouts on the current workspace to default", "M-C-<Space>"), setLayout $ XMonad.layoutHook conf)
  , (("Shrink the master area", "M--"), sendMessage Shrink)
  , (("Expand the master area", "M-+"), sendMessage Expand)
  , (("Shrink the slave area", "M-S--"), sendMessage ShrinkSlave)
  , (("Expand the slave area", "M-S-+"), sendMessage ExpandSlave)
  , (("Increase window padding", "M-S-C-+"), modifyPadding' 5)
  , (("Decrease window padding", "M-S-C--"), modifyPadding' (-5))
  , (("Increase number of visible windows in tiled layouts", "M-C-+"), increaseLimit)
  , (("Decrease number of visible windows in tiled layouts", "M-C--"), decreaseLimit)
  , (("Minimize window", "M-m"), withFocused minimizeWindow)
  , (("Maximize last minimized window", "M-S-m"), withLastMinimized maximizeWindowAndFocus)
  , (("Toggle actual full-screen mode (toggle struts + toggle window padding and round corners)", "M-f"), toggleFullScreen')
  , (("Only toggle struts", "M-S-f"), sendMessage ToggleStruts)
  , (("Go to next window with the same class name as the focused one", "M-n"), nextMatchWithThis Forward className)
  , (("Go to previous window with the same class name as the focused one", "M-S-n"), nextMatchWithThis Backward className)
  , (("Rotate slave windows up", "M-r"), rotSlavesUp)
  , (("Rotate slave windows down", "M-S-r"), rotSlavesDown)
  , (("Rotate all windows up", "M-C-r"), rotAllUp)
  , (("Prompt for a project name and then switch to it; automatically creates a project if a new name is returned from the prompt", "M-w"), switchProjectPrompt switchWorkspacePrompt)
  , (("Prompts for a project name and then shifts the currently focused window to that project", "M-S-w"), shiftToProjectPrompt shiftToWorkspacePrompt)
  , (("Run project startup-hook", "M-s"), bindOn myProjectStartHooks)
  , (("Remove the current workspace", "M-d"), removeWorkspace)
  , (("Prompt for a workspace and remove it", "M-S-d"), withWorkspace deleteWorkspacePrompt removeWorkspaceByTag)
  , (("Go to workspace of chosen window", "M-b"), gotoMenuArgs ["-c", "-i", "-l", "20", "-h", "30", "-fn", myDmenuFont, "-p", "Go to window:"])
  , (("Bring chosen window to current workspace", "M-S-b"), bringMenuArgs ["-c", "-i", "-l", "20", "-h", "30", "-fn", myDmenuFont, "-p", "Fetch window:"])
  , (("Toggle borders of all windows in current workspace", "M-C-b"), withAllWindows toggleBorder)
  , (("Move the mouse cursor to the current screen's lower right corner", "M-S-p"), banishScreen LowerRight)
  , (("Open terminal scratchpad", "M-S-s"), namedScratchpadAction myScratchpads "terminal")
  , (("Open todo scratchpad", "M-S-t"), namedScratchpadAction myScratchpads "todo")
  , (("Open GridSelect", "M-g"), spawnSelected myGridSelectConfig ["firefox", "chromium"])
  , (("View all open windows with GridSelect", "M-S-g"), goToSelected def)
  , (("Open Firefox", "M-x"), spawn myBrowser)
  , (("Open Firefox in private mode", "M-S-x"), spawn $ myBrowser ++ " -private")
  , (("Search on Google", "M-C-g"), searchPrompt "Google" "https://www.google.com/search?channel=fs&client=ubuntu&q=")
  , (("Browse my GitHub repos", "M-C-S-g"), browseMyGitHubRepos)
  , (("Search on YouTube", "M-C-y"), searchPrompt "YouTube" "http://www.youtube.com/results?search_type=search_videos&search_query=")
  , (("Browse saved YouTube playlists", "M-C-p"), browseYTPlaylists)
  , (("Search on Wikipedia", "M-C-w"), searchPrompt "Wikipedia" "http://en.wikipedia.org/wiki/Special:Search?go=Go&search=")
  , (("Search Splice samples", "M-C-s"), searchPrompt "Splice" "https://splice.com/sounds/search?q=")
  , (("Search on Netflix", "M-C-n"), searchPrompt "Netflix" "https://www.netflix.com/search?q=")
  , (("Search xmonad-contrib modules", "M-C-x"), searchXMonadContrib)
  , (("Increase brightness level for laptop screen", "<XF86MonBrightnessUp>"), modifyBrightness IncreaseBrightness 5)
  , (("Decrease brightness level for laptop screen", "<XF86MonBrightnessDown>"), modifyBrightness DecreaseBrightness 5)
  , (("Increase volume level", "<XF86AudioRaiseVolume>"), spawn "pactl set-sink-volume @DEFAULT_SINK@ +1.5%")
  , (("Decrease volume level", "<XF86AudioLowerVolume>"), spawn "pactl set-sink-volume @DEFAULT_SINK@ -1.5%")
  , (("Take screenshot of all displays", "<Print>"), takeScreenshot $ Scrot AllDisplays defScrotName defScrotCmd)
  , (("Take screenshot of selected area", "M-<Print>"), takeScreenshot $ Scrot SelectedArea defScrotName defScrotCmd)
  , (("Open Quick-Paste menu", "M-C-S-p"), openQuickPasteMenu)
  , (("Spawn XMonad command helper", "M-C-h"), lookupXMonadCommands conf)
  , (("Search existing XMonad shortcuts", "M-S-C-h"), lookupXMonadShortcuts conf)
  , (("Search useful Linux commands and shortcuts", "M-C-l"), showUsefulLinuxCmds)
  , (("Show useful Linux tips", "M-S-C-l"), showUsefulLinuxTips)
  , (("Open pdf in developer-compendium folder", "M-o"), spawn "openpdf")
  , (("Mount or unmount external drives", "M-S-C-m"), spawn "/home/freestingo/scripts/nicdrives.sh")
  ]

lookupXMonadCommands :: XConfig Layout -> X ()
lookupXMonadCommands conf = myDmenuPrompt (myCenterDMonad' "Lookup XMonad command:" $ M.keys myXMonadCommands) handleCommand
  where myXMonadCommands = M.fromList $ fst <$> myKeysWithDescription conf
        handleCommand desc = case M.lookup desc myXMonadCommands of
          (Just n) -> sendLowNotification "XMonad Help Menu" $ "Press <b>" ++ sanitize n ++ "</b> to <b>" ++ detitlize desc ++ "</b>!"
          Nothing  -> sendLowNotification "XMonad Help Menu" $ "Looks like there's <b>no command</b> to <b>" ++ desc ++ "</b> yet!"

lookupXMonadShortcuts :: XConfig Layout -> X ()
lookupXMonadShortcuts conf = myDmenuPrompt (myCenterDMonad' "Lookup XMonad shortcut:" $ M.keys myXMonadShortcuts) handleShortcut
  where myXMonadShortcuts = M.fromList $ swap . fst <$> myKeysWithDescription conf
        handleShortcut shortcut = case M.lookup shortcut myXMonadShortcuts of
          (Just action) -> sendLowNotification "XMonad Help Menu" $ "Press <b>" ++ sanitize shortcut ++ "</b> to <b>" ++ detitlize action ++ "</b>!"
          Nothing       -> sendLowNotification "XMonad Help Menu" $ "Looks like there's <b>no action</b> assigned to <b>" ++ shortcut ++ "</b> yet!"

