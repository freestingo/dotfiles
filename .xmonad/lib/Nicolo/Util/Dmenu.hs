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

searchXMonadContrib :: X ()
searchXMonadContrib = myDmenuPrompt (myCenterDMonad "Search xmonad-contrib modules:" xmonadContribHistory xmonadContribs) doSearch
    where xmonadContribHistory = "/home/freestingo/Documents/suckless/dmenu-5.0/histfile-xmonadcontrib"
          doSearch = spawn . (dmenuSearch ++) . safeArg . ("https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/" ++) . toSearchTerm
          toSearchTerm = (++ ".html") . map (\x -> if x == '.' then '-' else x)

confirmLogout :: X ()
confirmLogout = do
    result <- D.menuArgs "dmenu" ["-c", "-i", "-h", "30", "-fn", myDmenuFont, "-p", "Confirm logout?"] ["No", "Yes"]
    when (result == "Yes") $ io exitSuccess

