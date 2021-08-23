-- xmonad config template from https://archives.haskell.org/code.haskell.org/xmonad/man/xmonad.hs
-- IMPORTS

import XMonad
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Applicative (liftA2)
-- import Control.Arrow
import Control.Monad
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO (Handle)
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatSnap
import qualified XMonad.Actions.FlexibleManipulate as Flex
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.GridSelect
import XMonad.Actions.GroupNavigation
import XMonad.Actions.Minimize
import XMonad.Actions.NoBorders
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.RotSlaves
import XMonad.Actions.Search as S
import XMonad.Actions.SpawnOn
import XMonad.Actions.Submap as SM
import XMonad.Actions.TagWindows
import XMonad.Actions.Warp
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Minimize
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ScreenCorners
import XMonad.Hooks.UrgencyHook
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows -- with custom `increaseLimit` and layout description modifier
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Theme
import XMonad.Prompt.Unicode
import XMonad.Util.Cursor
import qualified XMonad.Util.Dmenu as D
import qualified XMonad.Util.Dzen as DZ
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import qualified XMonad.Util.Paste as P
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes
import XMonad.Util.Run
import XMonad.Util.SpawnNamedPipe
import XMonad.Util.WindowState

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


------------------------------------------------------------------------

{-|
    FREE-RANGE VARIABLES
    home-grown variables for easy config editing and wellness
-}

myTerminal = "alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

-- could be overridden by XMonad.Layout.NoBorders
myBorderWidth = 4

-- mod1Mask = left alt
-- mod4Mask = windows key
myModMask = mod1Mask

myWorkspaces :: [String]
myWorkspaces = ["code","front","back","web","oncode","npo","chat","skype","fun"]

clickable :: String -> String
clickable ws = "<action=xdotool key alt+" ++ show index ++ ">" ++ ws ++ "</action>"
    where workspaceIndexes = M.fromList $ zip myWorkspaces [1..]
          index = fromJust $ M.lookup ws workspaceIndexes

myNormalBorderColor :: String
myNormalBorderColor  = "#1E2428"

myFocusedBorderColor :: String
myFocusedBorderColor = "#8494B8"

myGridSelectConfig :: HasColorizer a => GSConfig a
myGridSelectConfig = def { gs_font = "xft:Hasklug Nerd Font Mono:pixelsize=16:antialias=true:hinting=true"
                         , gs_cellheight = 30
                         , gs_cellwidth = 100
                         }

myBrowser :: String
myBrowser = "firefox"

myScratchpads = [ NS "terminal" spawnTerminal findTerminal manageTerminal
                , NS "todo" spawnTodo findTodo manageTodo
                ]
                  where spawnTerminal = "alacritty --title=Scratchpad"
                        findTerminal = title =? "Scratchpad"
                        manageTerminal = customFloating $ W.RationalRect (3/25) (3/25) (3/4) (3/4)

                        spawnTodo = "alacritty --title=TODO --command vim ~/Documents/oncode/projects/npo/todo"
                        findTodo = title =? "TODO"
                        manageTodo = customFloating $ W.RationalRect (1/9) (1/10) (3/4) (3/4)

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

myEmails :: M.Map String [(KeyMask, Char)]
myEmails = M.fromList
         . zip ["personal", "oncode", "npo"]
         . (fmap . fmap) toKeyBind
         $ ["n.traini1@yahoo.it", "nicolo.traini@oncode.it", "nicolo.traini@nposervices.com"]
           where toKeyBind c | c == '@'  = (mod5Mask, 'ò')
                             | otherwise = (noModMask, c)

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

-- utility function for escaping characters in strings destined to be arguments to terminal commands
safeArg :: String -> String
safeArg = ("\"" ++) . (++ "\"")

safeArgs :: [String] -> String
safeArgs = unwords . map safeArg

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
-- TODO the xdotool trick to switch to the `web` workspace is beyond stupid.
--      I haven't yet found another way to just spawn windows on arbitrary workspaces
--      (spoiler: `spawnOn` or `spawnAndDo` do not work).
myDmenuPrompt :: MonadIO m => m String -> String -> (String -> m ()) -> m ()
myDmenuPrompt dmenuM destinationWs process =
    do
      selection <- dmenuM
      unless (all isSpace selection) $ spawn ("xdotool key alt+" ++ show destWsIndex) >> process selection
          where destWsIndex = fromJust . M.lookup destinationWs . M.fromList . zip myWorkspaces $ [1..]

-- TODO for some reason, safeSpawn won't work when `FilePath` features parameters. Why?????
browseMyGitHubRepos :: X ()
browseMyGitHubRepos = myDmenuPrompt (myCenterDMonad "Open GitHub Repo:" githubHistory myRepos) "web" searchGitHub
    where githubHistory = "/home/freestingo/Documents/suckless/dmenu-5.0/histfile-github"
          searchGitHub repo | repo `elem` myRepos = spawn $ dmenuSearch ++ safeArg ("https://github.com/freestingo/" ++ repo)
                            | otherwise           = spawn $ dmenuSearch ++ safeArg ("https://github.com/search?q=" ++ repo)

searchPrompt :: String -> String -> X ()
searchPrompt website url = myDmenuPrompt suggestHistory "web" doSearch
        where suggestHistory = do
                                 suggestions <- runProcessWithInput "cat" [historyFile] ""
                                 myCenterDMonad promptMsg historyFile . reverse . lines $ suggestions
              promptMsg = "Search on " ++ website ++ ":"
              historyFile = "/home/freestingo/Documents/suckless/dmenu-5.0/histfile-" ++ map toLower website
              doSearch = spawn . (dmenuSearch ++) . safeArg . (url ++)

browseYTPlaylists :: X ()
browseYTPlaylists = myDmenuPrompt (myCenterDMonad "Open YouTube playlist:" ytPlaylistHistory $ M.keys myYoutubePlaylists) "web" handlePlaylist
    where ytPlaylistHistory = "/home/freestingo/Documents/suckless/dmenu-5.0/histfile-ytplaylists"
          handlePlaylist name = case M.lookup name myYoutubePlaylists of
            (Just n) -> spawn $ dmenuSearch ++ safeArg ("https://www.youtube.com/playlist?list=" ++ n)
            Nothing -> do
                         myCurrentWs <- withWindowSet (pure . W.currentTag)
                         myDmenuPrompt (myDialogDMonad ("Playlist '" ++ name ++ "' not found!") ["Ok"]) myCurrentWs (return . const ())

confirmLogout :: X ()
confirmLogout = do
    result <- D.menuArgs "dmenu" ["-c", "-i", "-h", "30", "-fn", myDmenuFont, "-p", "Confirm logout?"] ["No", "Yes"]
    when (result == "Yes") $ io exitSuccess

{-|
   Execute an X action to all windows in the current workspace.
   Inspired by:
      https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-WithAll.html
      https://stackoverflow.com/questions/47051557/how-can-i-get-the-count-of-windows-in-the-current-workspace
-}
withAllWindows :: (Window -> X ()) -> X ()
withAllWindows action = traverse_ action . W.index . windowset =<< get

------------------------------------------------------------------------

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys = liftA2 (<+>) myKeysOldSyntax myKeysEZConfig

myKeysOldSyntax conf@XConfig { XMonad.modMask = modm } = M.fromList $
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    -- mod-{k,j} -> Switch to physical/Xinerama screens 1 or 2
    -- mod-shift-{k,j} -> Move client to screen 1 or 2
 ++ [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_j, xK_k] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myKeysEZConfig conf = mkKeymap conf [
    -- Launch a terminal
      ("M-S-<Return>", spawn $ XMonad.terminal conf)
    -- Launch dmenu
    , ("M-p", spawn $ unwords [ "dmenu_run"
                              , "-c -l 10 -h 30"
                              , "-fn " ++ safeArg myDmenuFont
                              , "-hp " ++ myDmenuFavorites
                              , "-H ~/Documents/suckless/dmenu-5.0/histfile"
                              ]
      )
    -- Close focused window
    , ("M-S-c", kill)
    -- Move focus to the next non-boring window
    , ("M-<Tab>", BW.focusDown)
    -- Move focus to the previous non-boring window
    , ("M-S-<Tab>", BW.focusUp)
    -- Swap the focused window and the master window
    , ("M-<Return>", windows W.swapMaster)
    -- Push window back into tiling
    , ("M-t", withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ("M-,", sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ("M-.", sendMessage (IncMasterN (-1)))
    -- Quit xmonad (logout)
    , ("M-S-q", confirmLogout)
    -- Restart xmonad
    , ("M-q", spawn "xmonad --recompile; xmonad --restart")
    -- Go to next workspace
    , ("M-l", nextWS)
    -- Go to previous workspace
    , ("M-h", prevWS)
    -- Rotate through the available layout algorithms
    , ("M-<Space>", sendMessage NextLayout)
    -- Reset the layouts on the current workspace to default
    , ("M-C-<Space>", setLayout $ XMonad.layoutHook conf)
    -- Shrink the master area
    , ("M--", sendMessage Shrink)
    -- Expand the master area
    , ("M-+", sendMessage Expand)
    -- Increase window padding
    , ("M-S-+", modifyPadding 5)
    -- Decrease window padding
    , ("M-S--", modifyPadding (-5))
    -- Increase number of visible windows in tiled layouts
    , ("M-C-+", increaseLimit)
    -- Decrease number of visible windows in tiled layouts
    , ("M-C--", decreaseLimit)
    -- Minimize window
    , ("M-m", withFocused minimizeWindow)
    -- Maximize last minimized window
    , ("M-S-m", withLastMinimized maximizeWindowAndFocus)
    -- Toggle actual full-screen mode (toggle struts + toggle window padding and round corners)
    , ("M-f", toggleFullScreen)
    -- Only toggle struts
    , ("M-S-f", sendMessage ToggleStruts)
    -- Go to next window with the same class name as the focused one
    , ("M-n", nextMatchWithThis Forward className)
    -- Go to previous window with the same class name as the focused one
    , ("M-S-n", nextMatchWithThis Backward className)
    -- Rotate slave windows up (useful when combined with `limitWindows`)
    , ("M-r", rotSlavesUp)
    -- Rotate slave windows down
    , ("M-S-r", rotSlavesDown)
    -- Rotate all windows up
    , ("M-C-r", rotAllUp)
    -- Prompt for a project name and then switch to it;
    -- Automatically creates a project if a new name is returned from the prompt
    , ("M-w", switchProjectPrompt switchWorkspacePrompt)
    -- Prompts for a project name and then shifts the currently focused window to that project
    , ("M-S-w", shiftToProjectPrompt shiftToWorkspacePrompt)
    -- Run project startup-hook
    , ("M-s", bindOn myProjectStartHooks)
    -- Remove the current workspace
    , ("M-d", removeWorkspace)
    -- Prompt for a workspace and remove it
    , ("M-S-d", withWorkspace deleteWorkspacePrompt removeWorkspaceByTag)
    -- Go to workspace of chosen window
    , ("M-b", gotoMenuArgs ["-c", "-i", "-l", "20", "-h", "30", "-fn", myDmenuFont, "-p", "Go to window:"])
    -- Bring chosen window to current workspace
    , ("M-S-b", bringMenuArgs ["-c", "-i", "-l", "20", "-h", "30", "-fn", myDmenuFont, "-p", "Fetch window:"])
    -- Toggle borders of all windows in current workspace
    , ("M-C-b", withAllWindows toggleBorder)
    -- Move mouse cursor to a corner of the focused window
    , ("M-S-p", banishScreen LowerRight)
    -- Open terminal scratchpad
    , ("M-S-s", namedScratchpadAction myScratchpads "terminal")
    -- Open todo scratchpad
    , ("M-S-t", namedScratchpadAction myScratchpads "todo")
    -- Open GridSelect
    , ("M-g", spawnSelected myGridSelectConfig ["firefox", "chromium"])
    -- View all open windows with GridSelect
    , ("M-S-g", goToSelected def)
    -- Open Firefox
    , ("M-x", spawn myBrowser)
    -- Open Firefox in private mode
    , ("M-S-x", spawn $ myBrowser ++ " -private")
    -- Open Chromium
    , ("M-c", spawn "chromium")
    -- Search on Google
    , ("M-C-g", searchPrompt "Google" "https://www.google.com/search?channel=fs&client=ubuntu&q=")
    -- Browse my GitHub repos
    , ("M-C-S-g", browseMyGitHubRepos)
    -- Search on YouTube
    , ("M-C-y", searchPrompt "YouTube" "http://www.youtube.com/results?search_type=search_videos&search_query=")
    -- Browse saved YouTube playlists
    , ("M-C-p", browseYTPlaylists)
    -- Search on Wikipedia
    , ("M-C-w", searchPrompt "Wikipedia" "http://en.wikipedia.org/wiki/Special:Search?go=Go&search=")
    -- Search Splice samples
    , ("M-C-s", searchPrompt "Splice" "https://splice.com/sounds/search?q=")
    -- Search on Netflix
    , ("M-C-n", searchPrompt "Netflix" "https://www.netflix.com/search?q=")
    -- Increase brightness level for laptop screen
    , ("<XF86MonBrightnessUp>", spawn "lux -a 5%")
    -- Decrease brightness level for laptop screen
    , ("<XF86MonBrightnessDown>", spawn "lux -s 5%")
    -- Increase volume level
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +1.5%")
    -- Decrease volume level
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -1.5%")
    -- Take screenshot of all displays
    , ("<Print>", spawn $ "scrot -e " ++ scrotCmd)
    -- Take screenshot of selected area
    , ("M-<Print>", spawn $ "scrot --line style=solid,width=2,color=\"red\" --select -e " ++ scrotCmd)
    -- Take screenshot of currently focused window
    , ("M-S-<Print>", spawn $ "scrot -u -e " ++ scrotCmd)
    -- Paste my personal email address
    , ("M-e p", mapM_ (uncurry P.pasteChar) $ fromJust $ M.lookup "personal" myEmails)
    -- Paste my oncode email address
    , ("M-e o", mapM_ (uncurry P.pasteChar) $ fromJust $ M.lookup "oncode" myEmails)
    -- Paste my NPO email address
    , ("M-e n", mapM_ (uncurry P.pasteChar) $ fromJust $ M.lookup "npo" myEmails)
  ]
        where scrotCmd = "'notify-send -u low \"scrot\" \"Saved screenshot <b>\\\"screen_%y-%m-%d_$wx$h.png\\\"</b> to <b>~/Pictures</b>!\" & mv $f ~/Pictures/screen_%y-%m-%d_$wx$h.png'"
              modifyWindowPadding x (Border t b l r) = Border (t + x) b       (l + x) r
              modifyScreenPadding x (Border t b l r) = Border t       (b + x) l       (r + x)
              modifyPadding amount = do
                  sendMessage $ ModifyWindowBorder (modifyWindowPadding amount)
                  sendMessage $ ModifyScreenBorder (modifyScreenPadding amount)
              toggleFullScreen = do
                  withAllWindows $ toggleBorder <> toggleTag
                  toggleScreenSpacingEnabled
                  toggleWindowSpacingEnabled
                  sendMessage ToggleStruts
                  where toggleTag win = do
                          hasNoRoundCorners <- hasTag "no-round-corners" win
                          if hasNoRoundCorners
                            then delTag "no-round-corners" win
                            else addTag "no-round-corners" win

------------------------------------------------------------------------

{-|
    Default actions bound to mouse events.
    You may also bind events to the mouse scroll wheel (button4 and button5)
-}

myMouseBindings XConfig { XMonad.modMask = modm } = M.fromList

    -- Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w
                           >> mouseMoveWindow w
                           >> ifClick (snapMagicMove (Just 50) (Just 50) w))

    , ((modm .|. shiftMask, button1), \w -> focus w
                                         >> mouseMoveWindow w
                                         >> ifClick (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w))

    -- Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w
                           >> windows W.shiftMaster)

    -- Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w
                           >> Flex.mouseResizeWindow w >> windows W.shiftMaster)
    ]

------------------------------------------------------------------------

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
         -- First padding value is screen-related, second one is windows-related
         -- https://www.reddit.com/r/xmonad/comments/n05z0o/questions_about_gaps_and_multimonitor/
         $ renamed [CutWordsLeft 1] $ spacingRaw False (Border 0 40 0 40) True (Border 40 0 40 0) True
         $ avoidStruts
         $ onWorkspace "skype" (renamed [Replace "One Window Max"] (limitWindows 1 Full))
         $ BW.boringAuto
         (   renamed [Replace "Simplest"] (minimize Simplest)
         ||| tiled
         ||| Mirror tiled
         )
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled = limitWindows 2 $ Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio = 1/2
     -- Percent of screen to increment by when resizing panes
     delta = 3/100

------------------------------------------------------------------------

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
      [ className =? "Skype"                        --> doShift "skype" ]
    , [ isDialog                                    --> doSideFloat C ]
    , [ className =? fc                             --> doCenterFloat | fc <- myFloatClasses ]
    , [ resource  =? ir                             --> doIgnore | ir <- myIgnoreResources ]
    ])
    <+> namedScratchpadManageHook myScratchpads
      where skypePopupSize = "program specified minimum size: 207 by 207"
            myFloatClasses = ["Mplayer", "Gimp", "Java", "Pavucontrol", "Gnome-calculator"]
            myIgnoreResources = ["desktop_window", "kdesktop"]
            --viewShift = doF . liftM2 (.) W.greedyView W.shift

shiftToAndNotify :: WorkspaceId -> ManageHook
shiftToAndNotify ws = do
                        cw <- currentWs
                        unless (cw == ws) sendNotification
                        doShift ws
                  where title = "XMonad LayoutHook"
                        body = "Moved window to <b>" ++ ws ++ "</b> workspace!"
                        sendNotification = spawn $ "notify-send -u low " ++ safeArgs [title, body]

------------------------------------------------------------------------

{-|
    Event handling

    * EwmhDesktops users should change this to ewmhDesktopsEventHook

    Defines a custom handler function for X Events. The function should
    return (All True) if the default handler is to be run afterwards. To
    combine event hooks use mappend or mconcat from Data.Monoid.
-}

myHandleEventHook = dynamicTitle myDynamicPropertyHook
                <+> (minimizeEventHook >> screenCornerEventHook)
              where myDynamicPropertyHook = composeAll [
                                    (("WhatsApp Web" `isInfixOf`) <$> title)
                               <||> (("Telegram Web" `isInfixOf`) <$> title) --> shiftToAndNotify "chat"
                              ]

------------------------------------------------------------------------

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

------------------------------------------------------------------------

{-|
    Startup hook

    Perform an arbitrary action each time xmonad starts or is restarted
    with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
    per-workspace layout choices.

    By default, do nothing.
-}
-- TODO check if the trailing `&`s are really needed
myStartupHook = do
        spawnOnce "nitrogen --restore &"
        spawnOnce "/usr/local/bin/picom --experimental-backends &"
        -- spawnOnce $ "picom --experimental-backends"
        --          ++ " --blur-background --blur-method gaussian --blur-kern 11x11gaussian"
        --          ++ " --xrender-sync-fence"
        -- manually setting latitude and longitude values because auto-location detection won't work for some reason
        spawnOnce "redshift-gtk -l 42.907546:13.882904 &"
        spawnOnce "~/scripts/startupcmds.sh &"
        spawnOnce "nm-applet &"
        spawnOnce "volumeicon &"
        spawnOnce "pasystray &"
        spawnOnce "trayer --edge top --align right --widthtype request --padding 5 --SetDockType true --SetPartialStrut false --expand true --monitor 1 --transparent true --alpha 0 --tint 0x1b1c22 --height 25"
        dynStatusBarStartup dynXmobar (return ())
        setWMName "LG3D"
        setDefaultCursor xC_left_ptr -- never show `X` shaped pointer, but use normal arrow pointer instead

dynXmobar :: ScreenId -> IO Handle
dynXmobar (S i) = do
                             -- debugging utilities
                             -- spawn $ "notify-send -u low \"screenID: " ++ monitor ++ "\""
                             -- spawn $ "notify-send -u low \"pipeCmd: " ++ pipeCmd ++ "\""
                             spawnPipe pipeCmd
                       where monitor = show i
                             pipeCmd = "xmobar -x " ++ monitor ++ " /home/freestingo/.config/xmobar/xmobarrc" ++ monitor

------------------------------------------------------------------------

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
    , Project { projectName = "skype"
              , projectDirectory = "~/"
              , projectStartHook = Nothing
              }
    ]

startChat :: X ()
startChat = spawn "firefox -new-window https://web.whatsapp.com/"

startSkype :: X ()
startSkype = spawn "skypeforlinux"

startOncode :: X ()
startOncode = do
                spawn "chromium --new-window teams.microsoft.com"
                spawn "chromium --new-window outlook.office.com/mail/inbox"
                spawn "chromium --new-window \"https://gitlab.com/oncodeit/oeds/fmc\""

startNpo :: X ()
startNpo = do
             spawn "teams"
             spawn $ myBrowser ++ " -new-window outlook.office.com/mail/inbox"

myProjectStartHooks :: [(String, X ())]
myProjectStartHooks = [("oncode", startOncode), ("npo", startNpo), ("chat", startChat), ("skype", startSkype)]
    where startChat = spawn "firefox -new-window https://web.whatsapp.com/"
          startSkype = spawn "skypeforlinux"
          startOncode = do
                          spawn "chromium --new-window teams.microsoft.com"
                          spawn "chromium --new-window outlook.office.com/mail/inbox"
                          spawn "chromium --new-window \"https://gitlab.com/oncodeit/oeds/fmc\""
          startNpo = do
                       spawn "teams"
                       spawn $ myBrowser ++ " -new-window outlook.office.com/mail/inbox"

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
defaults = def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myHandleEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

