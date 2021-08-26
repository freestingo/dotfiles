module Nicolo.Util.Functions where

import           Control.Monad
import           Data.Foldable
import           XMonad
import qualified XMonad.StackSet as W
import           XMonad.Hooks.ManageHelpers
import           Nicolo.Util.CustomVariables

-- utility function for escaping characters in strings destined to be arguments to terminal commands
safeArg :: String -> String
safeArg = ("\"" ++) . (++ "\"")

safeArgs :: [String] -> String
safeArgs = unwords . map safeArg

