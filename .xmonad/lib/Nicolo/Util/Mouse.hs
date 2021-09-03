module Nicolo.Util.Mouse where

import           Data.Map
import           XMonad
import qualified XMonad.Actions.FlexibleManipulate as Flex
import qualified XMonad.Actions.FlexibleResize as Flex
import           XMonad.Actions.FloatSnap
import qualified XMonad.Layout.BoringWindows as BW
import qualified XMonad.StackSet as W

{-|
    Default actions bound to mouse events.
    You may also bind events to the mouse scroll wheel (button4 and button5)
-}
myMouseBindings :: XConfig l -> Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig { XMonad.modMask = modm } = fromList

    -- Set the window to floating mode and move by dragging;
    -- snap position to surrounding windows when clicking
    [ ( (modm, button1)
      , \w -> focus w
           >> mouseMoveWindow w
           >> ifClick (snapMagicMove (Just 50) (Just 50) w)
      )

    -- Raise the window to the top of the stack
    , ( (modm, button2)
      , \w -> focus w
           >> windows W.shiftMaster
      )

    -- Set the window to floating mode and resize by dragging;
    -- snap resize to surrounding windows when clicking
    , ( (modm, button3)
      , \w -> focus w
           >> Flex.mouseResizeWindow w
           >> windows W.shiftMaster
           >> ifClick (snapMagicResize [L, R, U, D] (Just 50) (Just 50) w)
      )

    -- Move focus to the next non-boring window
    , ( (modm, button4)
      , const BW.focusUp
      )

    -- Move focus to the preivous non-boring window
    , ( (modm, button5)
      , const BW.focusDown
      )
    ]

