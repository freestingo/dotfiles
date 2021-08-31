module Nicolo.Util.Keys where

import qualified Data.Map as M
import           Data.Maybe
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
import           XMonad.Layout.Spacing
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import qualified XMonad.Util.Paste as P

import           Nicolo.Actions.Custom
import           Nicolo.Hooks.DynamicProjects
import           Nicolo.Util.CustomVariables
import           Nicolo.Util.Functions
import           Nicolo.Util.Dmenu
import           Nicolo.Util.Keys.Custom
import           Nicolo.Util.Prompt
import           Nicolo.Util.Scrot

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys = liftA2 (<+>) myKeysOldSyntax myKeysEZConfig

myKeysOldSyntax :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
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

myKeysEZConfig :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeysEZConfig conf = mkKeymap conf $ map extractKeyMap (myKeysWithDescription conf)
    where extractKeyMap ((_, keys), action) = (keys, action)

