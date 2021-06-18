-- xmonad config template from https://archives.haskell.org/code.haskell.org/xmonad/man/xmonad.hs
-- IMPORTS

import XMonad
import Data.Monoid
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
import XMonad.Actions.Search as S
import XMonad.Actions.Submap as SM
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Minimize
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import qualified XMonad.Layout.BoringWindows as BW
import XMonad.Layout.LimitWindows
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Unicode
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


------------------------------------------------------------------------
-- FREE-RANGE VARIABLES
-- home-grown variables for easy config editing and wellness

myTerminal      = "gnome-terminal"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

-- could be overridden by XMonad.Layout.NoBorders
myBorderWidth = 2

-- mod1Mask = left alt
-- mod4Mask = windows key
myModMask = mod1Mask

myWorkspaces :: [String]
myWorkspaces = ["front","back","web","oncode","npo","todo","chat","skype","fun"]

myNormalBorderColor :: String
myNormalBorderColor  = "#1E2428"

myFocusedBorderColor :: String
myFocusedBorderColor = "#A6D67C"

myGridSelectConfig :: HasColorizer a => GSConfig a
myGridSelectConfig = defaultGSConfig { gs_font = "xft:Hasklug Nerd Font Mono:pixelsize=16:antialias=true:hinting=true"
                                     , gs_cellheight = 30
                                     , gs_cellwidth = 100
                                     }

-- myDoFullFloat :: ManageHook
-- myDoFullFloat = doF W.focusDown <+> doFullFloat

myBrowser :: String
myBrowser = "firefox"

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
                             , defaultPrompter = \x -> promptMsg
                             }

searchEngineMap method = M.fromList $
      [ ((0, xK_g), method S.google)
      , ((0, xK_h), method S.hoogle)
      , ((0, xK_i), method S.images)
      , ((0, xK_m), method S.maps)
      , ((0, xK_w), method S.wikipedia)
      , ((0, xK_y), method S.youtube)
      ]

------------------------------------------------------------------------
-- KEY BINDINGS

myKeysOldSyntax conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm, xK_p), spawn "dmenu_run")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p), spawn "gmrun")

    -- Search commands
    , ((modm, xK_s), SM.submap $ searchEngineMap $ S.promptSearch $ styledPrompt "Search: ")
    , ((modm .|. shiftMask, xK_s), SM.submap $ searchEngineMap $ S.selectSearch)

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

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

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
    -- Remove the current workspace
    , ("M-d", removeWorkspace)
    -- Prompt for a workspace and remove it
    , ("M-S-d", withWorkspace deleteWorkspacePrompt removeWorkspaceByTag)
    -- Open GridSelect
    , ("M-g", spawnSelected myGridSelectConfig ["firefox", "chromium"])
    -- Open Firefox
    , ("M-x", spawn myBrowser)
    -- Open Firefox in private mode
    , ("M-S-x", spawn $ myBrowser ++ " -private")
    -- Open Chromium
    , ("M-c", spawn "chromium")
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

myKeys conf = (myKeysNewSyntax conf) <+> (myKeysOldSyntax conf)

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w
                            >> mouseMoveWindow w
                            >> ifClick (snapMagicMove (Just 50) (Just 50) w)))

    , ((modm .|. shiftMask, button1), (\w -> focus w
                                          >> mouseMoveWindow w
                                          >> ifClick (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w
                            >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w
                            >> Flex.mouseResizeWindow w >> windows W.shiftMaster))

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
myLayout = avoidStruts $ noBorders ((renamed [Replace "MyFull"] $ minimize Full) ||| tabbed shrinkText (theme darkTheme) ||| tiled ||| Mirror tiled)
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
myManageHook = composeAll
    [ className =? "MPlayer"                      --> doFloat
    , className =? "Gimp"                         --> doFloat
    , className =? "Skype"                        --> doShift "skype"
    , title     =? "Microsoft Teams Notification" --> doSideFloat NC
    -- , isFullscreen                                --> myDoFullFloat
    , resource  =? "desktop_window"               --> doIgnore
    , resource  =? "kdesktop"                     --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myHandleEventHook = minimizeEventHook

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
        spawnOnce "compton &"
        spawnOnce "~/scripts/startupcmds.sh &"
        spawnOnce "nm-applet &"
        spawnOnce "volumeicon &"
        spawnOnce "trayer --edge top --align right --widthtype request --padding 5 --SetDockType true --SetPartialStrut false --expand true --monitor 1 --transparent true --alpha 0 --tint 0x1b1c22 --height 25"
        setWMName "LG3D"
        setDefaultCursor xC_left_ptr -- never show `X` pointer, but use normal arrow pointer instead

------------------------------------------------------------------------

-- DYNAMIC PROJECTS!
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-DynamicProjects.html

projects :: [Project]
projects =
    [ Project { projectName = "web"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do spawn myBrowser
              }
    , Project { projectName = "oncode"
              , projectDirectory = "~/Documents/oncode/"
              , projectStartHook = Just $ do spawn "chromium --new-window teams.microsoft.com"
                                             spawn "chromium --new-window outlook.office.com/mail/inbox"
              }
    , Project { projectName = "npo"
              , projectDirectory = "~/Documents/oncode/projects/npo/"
              , projectStartHook = Just $ do spawn "teams"
                                             spawn $ myBrowser ++ " -new-window outlook.office.com/mail/inbox"
              }
    , Project { projectName = "chat"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do spawn $ myBrowser ++ " -new-window https://web.whatsapp.com/"
              }
    , Project { projectName = "skype"
              , projectDirectory = "~/"
              , projectStartHook = Just $ do spawn "skypeforlinux"
              }
    ]

------------------------------------------------------------------------

-- MAIN

main = do
  xmproc0 <- spawnPipe "xmobar -x 0 /home/freestingo/.config/xmobar/xmobarrc0"
  xmproc1 <- spawnPipe "xmobar -x 1 /home/freestingo/.config/xmobar/xmobarrc1"
  xmonad $ dynamicProjects projects $ docks defaults {
      logHook = dynamicLogWithPP $ xmobarPP
          { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
          , ppCurrent = xmobarColor "#A6D67C" "" . wrap "[" "]" -- current workspace
          , ppVisible = xmobarColor "#FCDF77" ""                -- visible but not current workspace
          , ppHidden = xmobarColor "#C792EA" ""                 -- hidden workspaces with windows
          , ppHiddenNoWindows = xmobarColor "#82AAFF" ""        -- hidden workspaces with no windows
          , ppTitle = xmobarColor "#B3AFC2" "" . shorten 70     -- title of active window
          , ppSep = "  "                                        -- separators
          , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- urgent workspace
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
