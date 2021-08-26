module Nicolo.Util.Dmenu where

import           Control.Applicative (liftA2)
import           Control.Monad
import           Data.Char
import qualified Data.Map as M
import           Data.Maybe
import           System.Exit
import           XMonad
import           XMonad.Actions.SpawnOn
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Dmenu as D
import qualified XMonad.Util.Dzen as DZ
import           XMonad.Util.Run

import           Nicolo.Actions.Custom
import           Nicolo.Util.CustomVariables
import           Nicolo.Util.Functions

-- TODO these should be persisted somewhere
myRepos :: [String]
myRepos = [ "dotfiles"
          , "hangman"
          , "haskell-programming-from-first-principles"
          , "nvcode-color-schemes.vim"
          , "rate-repository-app"
          , "apollo-graphql-library"
          , "ultimate-hooks"
          , "bloglist-frontend"
          , "unicafe-redux"
          , "anecdotes-redux"
          , "bloglist"
          , "ferdybot"
          , "helsinki-full-stack"
          , "briscola"
          , "phonebook"
          , "helsinki-full-stack-exercises"
          ]

myYoutubePlaylists :: M.Map String String
myYoutubePlaylists = M.fromList
    [ ("Watch Later", "WL")
    , ("oncode", "PLhLV2yWt3oZoL_aPcTR0EeBfuVEJ9D_r2")
    , ("sassi", "PLhLV2yWt3oZqeaba9fNNOM08ffzTCqhV6")
    , ("scavi", "PLhLV2yWt3oZoicsLvk1tm3pqSok5AVK4b")
    , ("Favorites", "FLhGrNvsmJwVyNVl-IW4sm-Q")
    , ("scarufficore", "PLhLV2yWt3oZpVIlQ8wcE0xPeViagBKHWt")
    , ("pazzesco", "PLhLV2yWt3oZrcS_2hO-OC6ikF1O0ih41-")
    , ("Full NGRX Course 2020", "PLV-DQnYj14bRFWMmuT6ptSL4v5fxMJnOS")
    , ("Monoscopio RAI - Musica anni 60 e 70 restaurata per TelstarWeb", "PLVQgz4PvtVx32jQ9xAwxDhHcsiJs5Wsfu")
    , ("ZOMG ZUFALL!!!", "PLhLV2yWt3oZogxBIX5f0OEf2SXTRNFlB7")
    ]

-- TODO it would be nice to dynamically create this helper map by somehow
--      parsing a file in which you save all XMonad keybinds...
myXMonadCommands :: M.Map String String
myXMonadCommands = M.fromList
    [ ("Launch Alacritty", "M-C-Return")
    , ("Launch Dmenu", "M-p")
    , ("Close focused window", "M-c")
    , ("Close all windows in current workspace", "M-S-c")
    , ("Move focus to the next non-boring window", "M-Tab")
    , ("Move focus to the previous non-boring window", "M-S-Tab")
    , ("Swap the focused window and the master window", "M-Return")
    , ("Push floating window back into tiling", "M-t")
    , ("Increment the number of windows in the master area", "M-,")
    , ("Deincrement the number of windows in the master area", "M-.")
    , ("Swap current screen with next screen", "M-S-C-k")
    , ("Swap current screen with previous screen", "M-S-C-j")
    , ("Quit xmonad (logout)", "M-S-q")
    , ("Restart xmonad", "M-q")
    , ("Go to next workspace", "M-l")
    , ("Go to previous workspace", "M-h")
    , ("Rotate through the available layout algorithms", "M-Space")
    , ("Reset the layouts on the current workspace to default", "M-C-Space")
    , ("Shrink the master area", "M--")
    , ("Expand the master area", "M-+")
    , ("Shrink the slave area", "M-S--")
    , ("Expand the slave area", "M-S-+")
    , ("Increase window padding", "M-S-C-+")
    , ("Decrease window padding", "M-S-C--")
    , ("Increase number of visible windows in tiled layouts", "M-C-+")
    , ("Decrease number of visible windows in tiled layouts", "M-C--")
    , ("Minimize window", "M-m")
    , ("Maximize last minimized window", "M-S-m")
    , ("Toggle actual full-screen mode (toggle struts + toggle window padding and round corners)", "M-f")
    , ("Only toggle struts", "M-S-f")
    , ("Go to next window with the same class name as the focused one", "M-n")
    , ("Go to previous window with the same class name as the focused one", "M-S-n")
    , ("Rotate slave windows up; useful when combined with `limitWindows`", "M-r")
    , ("Rotate slave windows down", "M-S-r")
    , ("Rotate all windows up", "M-C-r")
    , ("Prompt for a project name and then switch to it (automatically creating it if it doesn't exist yet)", "M-w")
    , ("Prompts for a project name and then shifts the currently focused window to that project", "M-S-w")
    , ("Run project startup-hook", "M-s")
    , ("Remove the current workspace", "M-d")
    , ("Prompt for a workspace and remove it", "M-S-d")
    , ("Go to workspace of chosen window", "M-b")
    , ("Bring chosen window to current workspace", "M-S-b")
    , ("Toggle borders of all windows in current workspace", "M-C-b")
    , ("Move the mouse cursor to the current screen's lower right corner", "M-S-p")
    , ("Open terminal scratchpad", "M-S-s")
    , ("Open todo scratchpad", "M-S-t")
    , ("Open GridSelect", "M-g")
    , ("View all open windows with GridSelect", "M-S-g")
    , ("Open Firefox", "M-x")
    , ("Open Firefox in private mode", "M-S-x")
    , ("Search on Google", "M-C-g")
    , ("Browse my GitHub repos", "M-C-S-g")
    , ("Search on YouTube", "M-C-y")
    , ("Browse saved YouTube playlists", "M-C-p")
    , ("Search on Wikipedia", "M-C-w")
    , ("Search Splice samples", "M-C-s")
    , ("Search on Netflix", "M-C-n")
    , ("Increase brightness level for laptop screen", "XF86MonBrightnessUp")
    , ("Decrease brightness level for laptop screen", "XF86MonBrightnessDown")
    , ("Increase volume level", "XF86AudioRaiseVolume")
    , ("Decrease volume level", "XF86AudioLowerVolume")
    , ("Take screenshot of all displays", "Print")
    , ("Take screenshot of selected area", "M-Print")
    , ("Take screenshot of currently focused window", "M-S-Print")
    , ("Paste my personal email address", "M-e p")
    , ("Paste my oncode email address", "M-e o")
    , ("Paste my NPO email address", "M-e n")
    , ("Kill all windows in current workspace", "M-S-c")
    , ("Spawn XMonad command helper", "M-C-h")
    ]

xmonadContribs :: [String]
xmonadContribs = [
      "XMonad.Actions.AfterDrag"
    , "XMonad.Actions.BluetileCommands"
    , "XMonad.Actions.Commands"
    , "XMonad.Actions.ConstrainedResize"
    , "XMonad.Actions.CopyWindow"
    , "XMonad.Actions.CycleRecentWS"
    , "XMonad.Actions.CycleSelectedLayouts"
    , "XMonad.Actions.CycleWS"
    , "XMonad.Actions.CycleWindows"
    , "XMonad.Actions.CycleWorkspaceByScreen"
    , "XMonad.Actions.DeManage"
    , "XMonad.Actions.DwmPromote"
    , "XMonad.Actions.DynamicProjects"
    , "XMonad.Actions.DynamicWorkspaceGroups"
    , "XMonad.Actions.DynamicWorkspaceOrder"
    , "XMonad.Actions.DynamicWorkspaces"
    , "XMonad.Actions.FindEmptyWorkspace"
    , "XMonad.Actions.FlexibleManipulate"
    , "XMonad.Actions.FlexibleResize"
    , "XMonad.Actions.FloatKeys"
    , "XMonad.Actions.FloatSnap"
    , "XMonad.Actions.FocusNth"
    , "XMonad.Actions.GridSelect"
    , "XMonad.Actions.GroupNavigation"
    , "XMonad.Actions.KeyRemap"
    , "XMonad.Actions.Launcher"
    , "XMonad.Actions.LinkWorkspaces"
    , "XMonad.Actions.MessageFeedback"
    , "XMonad.Actions.Minimize"
    , "XMonad.Actions.MouseGestures"
    , "XMonad.Actions.MouseResize"
    , "XMonad.Actions.Navigation2D"
    , "XMonad.Actions.NoBorders"
    , "XMonad.Actions.OnScreen"
    , "XMonad.Actions.PerWorkspaceKeys"
    , "XMonad.Actions.PhysicalScreens"
    , "XMonad.Actions.Plane"
    , "XMonad.Actions.Promote"
    , "XMonad.Actions.RandomBackground"
    , "XMonad.Actions.RotSlaves"
    , "XMonad.Actions.Search"
    , "XMonad.Actions.ShowText"
    , "XMonad.Actions.SimpleDate"
    , "XMonad.Actions.SinkAll"
    , "XMonad.Actions.SpawnOn"
    , "XMonad.Actions.Submap"
    , "XMonad.Actions.SwapPromote"
    , "XMonad.Actions.SwapWorkspaces"
    , "XMonad.Actions.TagWindows"
    , "XMonad.Actions.TopicSpace"
    , "XMonad.Actions.TreeSelect"
    , "XMonad.Actions.UpdateFocus"
    , "XMonad.Actions.UpdatePointer"
    , "XMonad.Actions.Warp"
    , "XMonad.Actions.WindowBringer"
    , "XMonad.Actions.WindowGo"
    , "XMonad.Actions.WindowMenu"
    , "XMonad.Actions.WindowNavigation"
    , "XMonad.Actions.WithAll"
    , "XMonad.Actions.Workscreen"
    , "XMonad.Actions.WorkspaceCursors"
    , "XMonad.Actions.WorkspaceNames"
    , "XMonad.Config.Arossato"
    , "XMonad.Config.Azerty"
    , "XMonad.Config.Bepo"
    , "XMonad.Config.Bluetile"
    , "XMonad.Config.Desktop"
    , "XMonad.Config.Dmwit"
    , "XMonad.Config.Droundy"
    , "XMonad.Config.Gnome"
    , "XMonad.Config.Kde"
    , "XMonad.Config.Mate"
    , "XMonad.Config.Prime"
    , "XMonad.Config.Sjanssen"
    , "XMonad.Config.Xfce"
    , "XMonad.Doc.Configuring"
    , "XMonad.Doc.Developing"
    , "XMonad.Doc.Extending"
    , "XMonad.Hooks.CurrentWorkspaceOnTop"
    , "XMonad.Hooks.DebugEvents"
    , "XMonad.Hooks.DebugKeyEvents"
    , "XMonad.Hooks.DebugStack"
    , "XMonad.Hooks.DynamicBars"
    , "XMonad.Hooks.DynamicHooks"
    , "XMonad.Hooks.DynamicLog"
    , "XMonad.Hooks.DynamicProperty"
    , "XMonad.Hooks.EwmhDesktops"
    , "XMonad.Hooks.FadeInactive"
    , "XMonad.Hooks.FadeWindows"
    , "XMonad.Hooks.FloatNext"
    , "XMonad.Hooks.ICCCMFocus"
    , "XMonad.Hooks.InsertPosition"
    , "XMonad.Hooks.ManageDebug"
    , "XMonad.Hooks.ManageDocks"
    , "XMonad.Hooks.ManageHelpers"
    , "XMonad.Hooks.Minimize"
    , "XMonad.Hooks.Place"
    , "XMonad.Hooks.PositionStoreHooks"
    , "XMonad.Hooks.RefocusLast"
    , "XMonad.Hooks.RestoreMinimized"
    , "XMonad.Hooks.ScreenCorners"
    , "XMonad.Hooks.Script"
    , "XMonad.Hooks.ServerMode"
    , "XMonad.Hooks.SetWMName"
    , "XMonad.Hooks.ToggleHook"
    , "XMonad.Hooks.UrgencyHook"
    , "XMonad.Hooks.WallpaperSetter"
    , "XMonad.Hooks.WorkspaceByPos"
    , "XMonad.Hooks.WorkspaceHistory"
    , "XMonad.Hooks.XPropManage"
    , "XMonad.Layout.Accordion"
    , "XMonad.Layout.AutoMaster"
    , "XMonad.Layout.AvoidFloats"
    , "XMonad.Layout.BinaryColumn"
    , "XMonad.Layout.BinarySpacePartition"
    , "XMonad.Layout.BorderResize"
    , "XMonad.Layout.BoringWindows"
    , "XMonad.Layout.ButtonDecoration"
    , "XMonad.Layout.CenteredMaster"
    , "XMonad.Layout.Circle"
    , "XMonad.Layout.Column"
    , "XMonad.Layout.Combo"
    , "XMonad.Layout.ComboP"
    , "XMonad.Layout.Cross"
    , "XMonad.Layout.Decoration"
    , "XMonad.Layout.DecorationAddons"
    , "XMonad.Layout.DecorationMadness"
    , "XMonad.Layout.Dishes"
    , "XMonad.Layout.DragPane"
    , "XMonad.Layout.DraggingVisualizer"
    , "XMonad.Layout.Drawer"
    , "XMonad.Layout.Dwindle"
    , "XMonad.Layout.DwmStyle"
    , "XMonad.Layout.FixedColumn"
    , "XMonad.Layout.Fullscreen"
    , "XMonad.Layout.Gaps"
    , "XMonad.Layout.Grid"
    , "XMonad.Layout.GridVariants"
    , "XMonad.Layout.Groups"
    , "XMonad.Layout.Groups.Examples"
    , "XMonad.Layout.Groups.Helpers"
    , "XMonad.Layout.Groups.Wmii"
    , "XMonad.Layout.Hidden"
    , "XMonad.Layout.HintedGrid"
    , "XMonad.Layout.HintedTile"
    , "XMonad.Layout.IM"
    , "XMonad.Layout.IfMax"
    , "XMonad.Layout.ImageButtonDecoration"
    , "XMonad.Layout.IndependentScreens"
    , "XMonad.Layout.LayoutBuilder"
    , "XMonad.Layout.LayoutBuilderP"
    , "XMonad.Layout.LayoutCombinators"
    , "XMonad.Layout.LayoutHints"
    , "XMonad.Layout.LayoutModifier"
    , "XMonad.Layout.LayoutScreens"
    , "XMonad.Layout.LimitWindows"
    , "XMonad.Layout.MagicFocus"
    , "XMonad.Layout.Magnifier"
    , "XMonad.Layout.Master"
    , "XMonad.Layout.Maximize"
    , "XMonad.Layout.MessageControl"
    , "XMonad.Layout.Minimize"
    , "XMonad.Layout.Monitor"
    , "XMonad.Layout.Mosaic"
    , "XMonad.Layout.MosaicAlt"
    , "XMonad.Layout.MouseResizableTile"
    , "XMonad.Layout.MultiColumns"
    , "XMonad.Layout.MultiDishes"
    , "XMonad.Layout.MultiToggle"
    , "XMonad.Layout.MultiToggle.Instances"
    , "XMonad.Layout.MultiToggle.TabBarDecoration"
    , "XMonad.Layout.Named"
    , "XMonad.Layout.NoBorders"
    , "XMonad.Layout.NoFrillsDecoration"
    , "XMonad.Layout.OnHost"
    , "XMonad.Layout.OneBig"
    , "XMonad.Layout.PerScreen"
    , "XMonad.Layout.PerWorkspace"
    , "XMonad.Layout.PositionStoreFloat"
    , "XMonad.Layout.Reflect"
    , "XMonad.Layout.Renamed"
    , "XMonad.Layout.ResizableTile"
    , "XMonad.Layout.ResizeScreen"
    , "XMonad.Layout.Roledex"
    , "XMonad.Layout.ShowWName"
    , "XMonad.Layout.SimpleDecoration"
    , "XMonad.Layout.SimpleFloat"
    , "XMonad.Layout.Simplest"
    , "XMonad.Layout.SimplestFloat"
    , "XMonad.Layout.SortedLayout"
    , "XMonad.Layout.Spacing"
    , "XMonad.Layout.Spiral"
    , "XMonad.Layout.Square"
    , "XMonad.Layout.StackTile"
    , "XMonad.Layout.StateFull"
    , "XMonad.Layout.Stoppable"
    , "XMonad.Layout.SubLayouts"
    , "XMonad.Layout.TabBarDecoration"
    , "XMonad.Layout.Tabbed"
    , "XMonad.Layout.ThreeColumns"
    , "XMonad.Layout.ToggleLayouts"
    , "XMonad.Layout.TrackFloating"
    , "XMonad.Layout.TwoPane"
    , "XMonad.Layout.TwoPanePersistent"
    , "XMonad.Layout.WindowArranger"
    , "XMonad.Layout.WindowNavigation"
    , "XMonad.Layout.WindowSwitcherDecoration"
    , "XMonad.Layout.WorkspaceDir"
    , "XMonad.Layout.ZoomRow"
    , "XMonad.Prompt"
    , "XMonad.Prompt.AppLauncher"
    , "XMonad.Prompt.AppendFile"
    , "XMonad.Prompt.ConfirmPrompt"
    , "XMonad.Prompt.DirExec"
    , "XMonad.Prompt.Directory"
    , "XMonad.Prompt.Email"
    , "XMonad.Prompt.FuzzyMatch"
    , "XMonad.Prompt.Input"
    , "XMonad.Prompt.Layout"
    , "XMonad.Prompt.Man"
    , "XMonad.Prompt.Pass"
    , "XMonad.Prompt.RunOrRaise"
    , "XMonad.Prompt.Shell"
    , "XMonad.Prompt.Ssh"
    , "XMonad.Prompt.Theme"
    , "XMonad.Prompt.Unicode"
    , "XMonad.Prompt.Window"
    , "XMonad.Prompt.Workspace"
    , "XMonad.Prompt.XMonad"
    , "XMonad.Util.Cursor"
    , "XMonad.Util.CustomKeys"
    , "XMonad.Util.DebugWindow"
    , "XMonad.Util.Dmenu"
    , "XMonad.Util.Dzen"
    , "XMonad.Util.EZConfig"
    , "XMonad.Util.ExclusiveScratchpads"
    , "XMonad.Util.ExtensibleState"
    , "XMonad.Util.Font"
    , "XMonad.Util.Image"
    , "XMonad.Util.Invisible"
    , "XMonad.Util.Loggers"
    , "XMonad.Util.Loggers.NamedScratchpad"
    , "XMonad.Util.Minimize"
    , "XMonad.Util.NamedActions"
    , "XMonad.Util.NamedScratchpad"
    , "XMonad.Util.NamedWindows"
    , "XMonad.Util.NoTaskbar"
    , "XMonad.Util.Paste"
    , "XMonad.Util.PositionStore"
    , "XMonad.Util.PureX"
    , "XMonad.Util.Rectangle"
    , "XMonad.Util.RemoteWindows"
    , "XMonad.Util.Replace"
    , "XMonad.Util.Run"
    , "XMonad.Util.Scratchpad"
    , "XMonad.Util.SessionStart"
    , "XMonad.Util.SpawnNamedPipe"
    , "XMonad.Util.SpawnOnce"
    , "XMonad.Util.Stack"
    , "XMonad.Util.StringProp"
    , "XMonad.Util.Themes"
    , "XMonad.Util.Timer"
    , "XMonad.Util.TreeZipper"
    , "XMonad.Util.Types"
    , "XMonad.Util.Ungrab"
    , "XMonad.Util.WindowProperties"
    , "XMonad.Util.WindowState"
    , "XMonad.Util.WorkspaceCompare"
    , "XMonad.Util.XSelection"
    , "XMonad.Util.XUtils"
    ]

myDzenFont :: String
myDzenFont = "Hasklug Nerd Font Mono"

myDzenConfig :: DZ.DzenConfig
myDzenConfig = DZ.onCurr (DZ.center 500 55)
           >=> DZ.font myDzenFont
           >=> DZ.bgColor "#1B1C22"
           >=> DZ.fgColor "#D1D5DA"
           >=> DZ.timeout 2

myDmenuFont :: String
myDmenuFont = "Hasklug Nerd Font Mono"

myDmenuFavorites :: String
myDmenuFavorites = "pavucontrol,dbeaver-ce,gnome-terminal,chromium,qutebrowser,nautilus,poweroff"

dmenuSearch :: String
dmenuSearch = "firefox -new-window "

myCenterDMonad :: MonadIO m => String -> String -> [String] -> m String
myCenterDMonad promptMsg historyFile = D.menuArgs "dmenu" ["-i", "-l", "20", "-h", "30", "-fn", myDmenuFont, "-p", promptMsg, "-H", historyFile, "-x", "540", "-y", "290", "-z", "900"]

myInlineDMonad :: MonadIO m => String -> String -> [String] -> m String
myInlineDMonad promptMsg historyFile = D.menuArgs "dmenu" ["-i", "-h", "30", "-fn", myDmenuFont, "-p", promptMsg, "-H", historyFile, "-x", "500", "-y", "440", "-z", "900"]

myDialogDMonad :: MonadIO m => String -> [String] -> m String
myDialogDMonad promptMsg = D.menuArgs "dmenu" ["-i", "-h", "30", "-fn", myDmenuFont, "-p", promptMsg, "-x", "500", "-y", "440", "-z", "900"]

-- TODO functions written with `do` notation don't work properly when they're set
--      as a result of a submap-key combination. Why?
myDmenuPrompt :: MonadIO m => m String -> (String -> m ()) -> m ()
myDmenuPrompt dmenuM process = do
    selection <- dmenuM
    unless (all isSpace selection) $ process selection

-- TODO for some reason, safeSpawn won't work when `FilePath` features parameters. Why?????
browseMyGitHubRepos :: X ()
browseMyGitHubRepos = myDmenuPrompt (myCenterDMonad "Open GitHub Repo:" githubHistory myRepos) searchGitHub
    where githubHistory = "/home/freestingo/Documents/suckless/dmenu-5.0/histfile-github"
          searchGitHub repo | repo `elem` myRepos = spawn $ dmenuSearch ++ safeArg ("https://github.com/freestingo/" ++ repo)
                            | otherwise           = spawn $ dmenuSearch ++ safeArg ("https://github.com/search?q=" ++ repo)

searchPrompt :: String -> String -> X ()
searchPrompt website url = myDmenuPrompt suggestHistory doSearch
        where suggestHistory = do
                                 suggestions <- runProcessWithInput "cat" [historyFile] ""
                                 myCenterDMonad promptMsg historyFile . reverse . lines $ suggestions
              promptMsg = "Search on " ++ website ++ ":"
              historyFile = "/home/freestingo/Documents/suckless/dmenu-5.0/histfile-" ++ map toLower website
              doSearch = spawn . (dmenuSearch ++) . safeArg . (url ++)

browseYTPlaylists :: X ()
browseYTPlaylists = myDmenuPrompt (myCenterDMonad "Open YouTube playlist:" ytPlaylistHistory $ M.keys myYoutubePlaylists) handlePlaylist
    where ytPlaylistHistory = "/home/freestingo/Documents/suckless/dmenu-5.0/histfile-ytplaylists"
          handlePlaylist name = case M.lookup name myYoutubePlaylists of
            (Just n) -> spawn $ dmenuSearch ++ safeArg ("https://www.youtube.com/playlist?list=" ++ n)
            Nothing  -> myDmenuPrompt (myDialogDMonad ("Playlist '" ++ name ++ "' not found!") ["Ok"]) (return . const ())

-- TODO this and `browseYTPlaylists could probably be refactored to one function`
lookupXMonadCommands :: X ()
lookupXMonadCommands = myDmenuPrompt (myCenterDMonad "Lookup XMonad command:" xmonadCmdsHistory $ M.keys myXMonadCommands) handleCommand
    where xmonadCmdsHistory = "/home/freestingo/Documents/suckless/dmenu-5.0/histfile-xmonadcmds"
          handleCommand name = case M.lookup name myXMonadCommands of
            (Just n) -> sendNotification "XMonad Help Menu" $ "Press <b>" ++ n ++ "</b> to <b>" ++ liftA2 (++) (map toLower . take 1) (drop 1) name ++ "</b>!"
            Nothing  -> sendNotification "XMonad Help Menu"
                      $ "Looks like there's <b>no command</b> to <b>"
                     ++ name
                     ++ "</b>!<br><br>Either you forgot it to add it to the <i><b>myXMonadCommands</b></i> list, or it simply hasn't been implemented yet."

searchXMonadContrib :: X ()
searchXMonadContrib = myDmenuPrompt (myCenterDMonad "Search xmonad-contrib modules:" xmonadContribHistory xmonadContribs) doSearch
    where xmonadContribHistory = "/home/freestingo/Documents/suckless/dmenu-5.0/histfile-xmonadcontrib"
          doSearch = spawn . (dmenuSearch ++) . safeArg . ("https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/" ++) . toSearchTerm
          toSearchTerm = (++ ".html") . map (\x -> if x == '.' then '-' else x)

confirmLogout :: X ()
confirmLogout = do
    result <- D.menuArgs "dmenu" ["-c", "-i", "-h", "30", "-fn", myDmenuFont, "-p", "Confirm logout?"] ["No", "Yes"]
    when (result == "Yes") $ io exitSuccess

