module Nicolo.Util.Scrot where

import Data.Char
import XMonad.Layout.LayoutModifier
import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Dmenu as D

{-|
   Scrot utilities
-}

data ScrotArea = AllDisplays | SelectedArea | FocusedWindow

instance Show ScrotArea where
    show AllDisplays = " "
    show SelectedArea = " --line style=solid,width=2,color=\"red\" -s -f "
    show FocusedWindow = " -u "

type ScrotName = (String -> String)

type ScrotCmd = (String -> String)

data Scrot = Scrot ScrotArea ScrotName ScrotCmd

defScrotName :: ScrotName
defScrotName s | all isSpace s = "screen_%y-%m-%d_$wx$h.png"
               | otherwise = "screen_%y-%m-%d_$wx$h_" ++ s ++ ".png"

defScrotCmd :: ScrotCmd
defScrotCmd s = "-e 'notify-send -u low \"scrot\" \"Saved screenshot <b>\\\"" ++ s ++ "\\\"</b> to <b>~/Pictures</b>!\" & mv $f ~/Pictures/" ++ s ++ "'"

takeScreenshot :: Scrot -> X ()
takeScreenshot (Scrot area name cmd) = do
    postfix <- D.menuArgs "dmenu" ["-i", "-l", "20", "-h", "30", "-fn", "Hasklug Nerd Font Mono", "-p", "Screenshot name:", "-x", "540", "-y", "290", "-z", "900"] []
    spawn $ "scrot" ++ show area ++ cmd (name postfix)

