module Nicolo.Hooks.StartupHook where

import System.IO (Handle)
import XMonad
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.SetWMName
import XMonad.Util.Cursor
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

{-|
    Startup hook

    Perform an arbitrary action each time xmonad starts or is restarted
    with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
    per-workspace layout choices.

    By default, do nothing.
-}
-- TODO check if the trailing `&`s are really needed
myStartupHook = do
        spawnOnce "nitrogen --restore"
        spawnOnce "/usr/local/bin/picom --experimental-backends"
        -- spawnOnce $ "picom --experimental-backends"
        --          ++ " --blur-background --blur-method gaussian --blur-kern 11x11gaussian"
        --          ++ " --xrender-sync-fence"
        -- manually setting latitude and longitude values because auto-location detection won't work for some reason
        spawnOnce "redshift-gtk -l 42.907546:13.882904"
        spawnOnce "~/scripts/startupcmds.sh"
        spawnOnce "nm-applet"
        spawnOnce "volumeicon"
        spawnOnce "pasystray"
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

