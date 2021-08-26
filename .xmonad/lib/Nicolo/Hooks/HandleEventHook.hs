module Nicolo.Hooks.HandleEventHook where

import Data.List
import XMonad
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.Minimize
import XMonad.Hooks.ScreenCorners
import XMonad.Layout.NoBorders

import Nicolo.Actions.Custom
import Nicolo.Hooks.DynamicProjects
import Nicolo.Util.Functions

{-|
    Event handling

    * EwmhDesktops users should change this to ewmhDesktopsEventHook

    Defines a custom handler function for X Events. The function should
    return (All True) if the default handler is to be run afterwards. To
    combine event hooks use mappend or mconcat from Data.Monoid.
-}
myHandleEventHook = dynamicTitle myDynamicPropertyHook
                <+> borderEventHook
                <+> minimizeEventHook
                <+> screenCornerEventHook
              where myDynamicPropertyHook = composeAll [
                                    (("WhatsApp Web" `isInfixOf`) <$> title)
                               <||> (("Telegram Web" `isInfixOf`) <$> title) --> shiftToAndNotify "chat"
                              ]

