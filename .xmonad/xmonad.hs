-- xmonad config template from https://archives.haskell.org/code.haskell.org/xmonad/man/xmonad.hs
-- IMPORTS

import XMonad
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Monad
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy)
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatSnap
import qualified XMonad.Actions.FlexibleManipulate as Flex
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.GridSelect
import XMonad.Actions.Minimize
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.Search as S
import XMonad.Actions.Submap as SM
import XMonad.Actions.Warp
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Minimize
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ScreenCorners
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.LimitWindows
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Theme
import XMonad.Prompt.Unicode
import XMonad.Util.Cursor
import qualified XMonad.Util.Dmenu as D
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


------------------------------------------------------------------------
-- FREE-RANGE VARIABLES
-- home-grown variables for easy config editing and wellness

myTerminal = "alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

-- could be overridden by XMonad.Layout.NoBorders
myBorderWidth = 3

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
myFocusedBorderColor = "#A6D67C"

myGridSelectConfig :: HasColorizer a => GSConfig a
myGridSelectConfig = def { gs_font = "xft:Hasklug Nerd Font Mono:pixelsize=16:antialias=true:hinting=true"
                         , gs_cellheight = 30
                         , gs_cellwidth = 100
                         }

-- myDoFullFloat :: ManageHook
-- myDoFullFloat = doF W.focusDown <+> doFullFloat

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

myDmenuFont :: String
myDmenuFont = "Hasklug Nerd Font Mono"

myCenterDMonad :: MonadIO m => String -> String -> [String] -> m String
myCenterDMonad promptMsg historyFile = D.menuArgs "dmenu" ["-i", "-l", "20", "-h", "30", "-fn", myDmenuFont, "-p", promptMsg, "-H", historyFile, "-x", "540", "-y", "290", "-z", "900"]

myInlineDMonad :: MonadIO m => String -> String -> [String] -> m String
myInlineDMonad promptMsg historyFile = D.menuArgs "dmenu" ["-i", "-h", "30", "-fn", myDmenuFont, "-p", promptMsg, "-H", historyFile, "-x", "500", "-y", "440", "-z", "900"]

myDialogDMonad :: MonadIO m => String -> [String] -> m String
myDialogDMonad promptMsg = D.menuArgs "dmenu" ["-i", "-h", "30", "-fn", myDmenuFont, "-p", promptMsg, "-x", "500", "-y", "440", "-z", "900"]

-- TODO functions written with `do` notation don't work properly when they're set as a result of a submap-key combination. Why????
myDmenuPrompt :: MonadIO m => m String -> (String -> m ()) -> m ()
myDmenuPrompt dmenuM process =
    do
      selection <- dmenuM
      unless (all isSpace selection) $ process selection

browseMyGitHubRepos :: X ()
browseMyGitHubRepos = myDmenuPrompt (myCenterDMonad "Open GitHub Repo:" githubHistory myRepos) searchGitHub
    where githubHistory = "/home/freestingo/Documents/suckless/dmenu-5.0/histfile-github"
          searchGitHub repo | repo `elem` myRepos = safeSpawn "qutebrowser" ["https://github.com/freestingo/" ++ repo]
                            | otherwise           = safeSpawn "qutebrowser" ["https://github.com/search?q=" ++ repo]

searchPrompt :: String -> String -> X ()
searchPrompt website url = myDmenuPrompt suggestHistory doSearch
        where suggestHistory = do
                                 suggestions <- runProcessWithInput "cat" [historyFile] ""
                                 myCenterDMonad promptMsg historyFile . reverse . lines $ suggestions
              promptMsg = "Search on " ++ website ++ ":"
              historyFile = "/home/freestingo/Documents/suckless/dmenu-5.0/histfile-" ++ map toLower website
              doSearch = safeSpawn "qutebrowser" . return . (url ++)

browseYTPlaylists :: X ()
browseYTPlaylists = myDmenuPrompt (myCenterDMonad "Open YouTube playlist:" ytPlaylistHistory $ M.keys myYoutubePlaylists) handlePlaylist
    where ytPlaylistHistory = "/home/freestingo/Documents/suckless/dmenu-5.0/histfile-ytplaylists"
          handlePlaylist name = case M.lookup name myYoutubePlaylists of
            (Just n) -> safeSpawn "qutebrowser" ["https://www.youtube.com/playlist?list=" ++ n]
            Nothing -> myDmenuPrompt (myDialogDMonad ("Playlist '" ++ name ++ "' not found!") ["Ok"]) (return . const ())

confirmLogout :: X ()
confirmLogout = do
    result <- D.menuArgs "dmenu" ["-c", "-i", "-h", "30", "-fn", myDmenuFont, "-p", "Confirm logout?"] ["No", "Yes"]
    when (result == "Yes") $ io exitSuccess


------------------------------------------------------------------------
-- KEY BINDINGS

myKeysOldSyntax conf@XConfig { XMonad.modMask = modm } = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm, xK_p), spawn $ unwords [ "dmenu_run"
                                     , "-c -l 10 -h 30"
                                     , "-fn \"Hasklug Nerd Font Mono\""
                                     , "-hp pavucontrol,dbeaver-ce,gnome-terminal,chromium,qutebrowser,poweroff"
                                     , "-H ~/Documents/suckless/dmenu-5.0/histfile"
                                     ]
      )

    -- -- launch gmrun
    -- , ((modm .|. shiftMask, xK_p), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c), kill)

     -- Rotate through the available layout algorithms
    , ((modm, xK_space), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm, xK_n), refresh)

    -- Move focus to the next window
    , ((modm, xK_Tab), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm .|. shiftMask, xK_Tab), windows W.focusUp)

    -- -- Move focus to the master window
    -- , ((modm, xK_m), windows W.focusMaster)

    -- Swap the focused window and the master window
    , ((modm, xK_Return), windows W.swapMaster)

    -- -- Swap the focused window with the next window
    -- , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- -- Swap the focused window with the previous window
    -- , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad (logout)
    , ((modm .|. shiftMask, xK_q     ), confirmLogout)

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{k,j} -> Switch to physical/Xinerama screens 1 or 2
    -- mod-shift-{k,j} -> Move client to screen 1 or 2
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_k, xK_j] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myKeysNewSyntax c = mkKeymap c [
    -- Go to next workspace
      ("M-l", nextWS)
    -- Go to previous workspace
    , ("M-h", prevWS)
    -- Shrink the master area
    , ("M--", sendMessage Shrink)
    -- Expand the master area
    , ("M-+", sendMessage Expand)
    -- Minimize window
    , ("M-m", withFocused minimizeWindow)
    -- Maximize last minimized window
    , ("M-S-m", withLastMinimized maximizeWindowAndFocus)
    -- Toggle actual full-screen mode (windows will overlap xmobar)
    , ("M-f", sendMessage ToggleStruts)
    -- Prompt for a project name and then switch to it;
    -- Automatically creates a project if a new name is returned from the prompt
    , ("M-w", switchProjectPrompt switchWorkspacePrompt)
    -- Prompts for a project name and then shifts the currently focused window to that project
    , ("M-S-w", shiftToProjectPrompt shiftToWorkspacePrompt)
    -- Run project startup-hook
    , ("M-s", bindOn [("oncode", startOncode), ("npo", startNpo), ("chat", startChat), ("skype", startSkype)])
    -- Remove the current workspace
    , ("M-d", removeWorkspace)
    -- Prompt for a workspace and remove it
    , ("M-S-d", withWorkspace deleteWorkspacePrompt removeWorkspaceByTag)
    -- Go to workspace of chosen window
    , ("M-b", gotoMenuArgs ["-c", "-i", "-l", "20", "-h", "30", "-fn", myDmenuFont, "-p", "Go to window:"])
    -- Bring chosen window to current workspace
    , ("M-S-b", bringMenuArgs ["-c", "-i", "-l", "20", "-h", "30", "-fn", myDmenuFont, "-p", "Fetch window:"])
    -- Move mouse cursor to a corner of the focused window
    , ("M-S-p", banish LowerRight)
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
    -- Browse my GitHub repos
    , ("M-C-r", browseMyGitHubRepos)
    -- Increase brightness level for laptop screen
    , ("<XF86MonBrightnessUp>", spawn "lux -a 5%")
    -- Decrease brightness level for laptop screen
    , ("<XF86MonBrightnessDown>", spawn "lux -s 5%")
    -- Increase volume level
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +1.5%")
    -- Decrease volume level
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -1.5%")
    -- Take screenshot of all displays
    , ("<Print>", spawn "scrot -e 'mv $f ~/Pictures/'")
    -- Take screenshot of selected area
    , ("M-<Print>", spawn "scrot --line style=solid,width=2,color=\"red\" --select -e 'mv $f ~/Pictures/'")
    -- Take screenshot of currently focused window
    , ("M-S-<Print>", spawn "scrot -u -e 'mv $f ~/Pictures/'")
  ]

myKeys conf = myKeysNewSyntax conf <+> myKeysOldSyntax conf

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings XConfig { XMonad.modMask = modm } = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w
                           >> mouseMoveWindow w
                           >> ifClick (snapMagicMove (Just 50) (Just 50) w))

    , ((modm .|. shiftMask, button1), \w -> focus w
                                         >> mouseMoveWindow w
                                         >> ifClick (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w
                           >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w
                           >> Flex.mouseResizeWindow w >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = screenCornerLayoutHook
         $ avoidStruts
         $ noBorders (   renamed [Replace "MyFull"] (minimize Full)
                     ||| tabbed shrinkText (theme darkTheme)
                     ||| tiled
                     ||| Mirror tiled
                     )
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- Check also https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips
myManageHook = composeAll (concat
    [
      [ className =? "Skype"                        --> doShift "skype" ]
    , [ className =? "qutebrowser"                  --> viewShift "web" ]
    , [ title     =? "Microsoft Teams Notification" --> doSideFloat NC ]
    , [ className =? fc                             --> doFloat | fc <- myFloatClasses ]
    , [ resource  =? fr                             --> doFloat | fr <- myFloatResources ]
    , [ resource  =? ir                             --> doIgnore | ir <- myIgnoreResources ]
    ])
    <+> namedScratchpadManageHook myScratchpads
      where viewShift = doF . liftM2 (.) W.greedyView W.shift
            myFloatClasses = ["Mplayer", "Gimp", "Skype", "Java"]
            myFloatResources = ["Dialog"]
            myIgnoreResources = ["desktop_window", "kdesktop"]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myHandleEventHook e = do
  minimizeEventHook e
  screenCornerEventHook e

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
        spawnOnce "nitrogen --restore &"
        spawnOnce $ "picom --experimental-backends"
                 ++ " --blur-background --blur-method gaussian --blur-kern 11x11gaussian"
                 ++ " --xrender-sync-fence"
        -- manually setting latitude and longitude values because auto-location detection won't work for some reason
        spawnOnce "redshift-gtk -l 42.907546:13.882904 &"
        spawnOnce "~/scripts/startupcmds.sh &"
        spawnOnce "nm-applet &"
        spawnOnce "volumeicon &"
        spawnOnce "trayer --edge top --align right --widthtype request --padding 5 --SetDockType true --SetPartialStrut false --expand true --monitor 1 --transparent true --alpha 0 --tint 0x1b1c22 --height 25"
        setWMName "LG3D"
        setDefaultCursor xC_left_ptr -- never show `X` shaped pointer, but use normal arrow pointer instead
        -- addScreenCorners [ (SCLowerRight, nextWS)
        --                  , (SCLowerLeft, prevWS)
        --                  ]

------------------------------------------------------------------------

-- DYNAMIC PROJECTS!
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-DynamicProjects.html

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

startNpo :: X ()
startNpo = do
             spawn "teams"
             spawn $ myBrowser ++ " -new-window outlook.office.com/mail/inbox"

------------------------------------------------------------------------

-- MAIN

main = do
  xmproc0 <- spawnPipe "xmobar -x 0 /home/freestingo/.config/xmobar/xmobarrc0"
  xmproc1 <- spawnPipe "xmobar -x 1 /home/freestingo/.config/xmobar/xmobarrc1"
  xmonad $ dynamicProjects projects $ docks defaults {
      logHook = dynamicLogWithPP $ xmobarPP
          { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
          , ppCurrent = xmobarColor "#A6D67C" "" . wrap "[" "]"         -- current workspace
          , ppVisible = xmobarColor "#FCDF77" "" . clickable            -- visible but not current workspace
          , ppHidden = xmobarColor "#C792EA" "" . clickable             -- hidden workspaces with windows
          , ppHiddenNoWindows = xmobarColor "#82AAFF" "" . clickable    -- hidden workspaces with no windows
          , ppTitle = xmobarColor "#B3AFC2" "" . shorten 70             -- title of active window
          , ppSep = "  "                                                -- separators
          , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"          -- urgent workspace
          }
    }

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myHandleEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
