module Nicolo.Actions.CycleWS ( nextWS
                              , prevWS
                              )
                              where

import Data.List (find, findIndex)
import Data.Maybe (isNothing, isJust)

import XMonad hiding (workspaces)
import qualified XMonad.Hooks.WorkspaceHistory as WH
import XMonad.StackSet hiding (filter)
import XMonad.Util.Types
import XMonad.Util.WorkspaceCompare

-- | Switch to the next workspace.
nextWS :: X ()
nextWS = switchWorkspace 1

-- | Switch to the previous workspace.
prevWS :: X ()
prevWS = switchWorkspace (-1)

switchWorkspace :: Int -> X ()
switchWorkspace d = wsBy d >>= windows . view

wsBy :: Int -> X WorkspaceId
wsBy = findWorkspace getSortByIndex Next AnyWS

-- | What type of workspaces should be included in the cycle?
data WSType = EmptyWS     -- ^ cycle through empty workspaces
            | NonEmptyWS  -- ^ cycle through non-empty workspaces
            | HiddenWS    -- ^ cycle through non-visible workspaces
            | HiddenNonEmptyWS  -- ^ cycle through non-empty non-visible workspaces
            | HiddenEmptyWS  -- ^ cycle through empty non-visible workspaces
            | AnyWS       -- ^ cycle through all workspaces
            | WSTagGroup Char
                          -- ^ cycle through workspaces in the same group, the
                          --   group name is all characters up to the first
                          --   separator character or the end of the tag
            | WSIs (X (WindowSpace -> Bool))
                          -- ^ cycle through workspaces satisfying
                          --   an arbitrary predicate

-- | Convert a WSType value to a predicate on workspaces.
wsTypeToPred :: WSType -> X (WindowSpace -> Bool)
wsTypeToPred EmptyWS    = return (isNothing . stack)
wsTypeToPred NonEmptyWS = return (isJust . stack)
wsTypeToPred HiddenWS   = do hs <- gets (map tag . hidden . windowset)
                             return (\w -> tag w `elem` hs)
wsTypeToPred HiddenNonEmptyWS  = do ne <- wsTypeToPred NonEmptyWS
                                    hi <- wsTypeToPred HiddenWS
                                    return (\w -> hi w && ne w)
wsTypeToPred HiddenEmptyWS  = do ne <- wsTypeToPred EmptyWS
                                 hi <- wsTypeToPred HiddenWS
                                 return (\w -> hi w && ne w)
wsTypeToPred AnyWS      = return (const True)
wsTypeToPred (WSTagGroup sep) = do cur <- (groupName.workspace.current) `fmap` gets windowset
                                   return $ (cur ==).groupName
                                   where groupName = takeWhile (/=sep).tag
wsTypeToPred (WSIs p)   = p

-- | View the next workspace in the given direction that satisfies
--   the given condition.
moveTo :: Direction1D -> WSType -> X ()
moveTo dir t = doTo dir t getSortByIndex (windows . greedyView)

-- | Move the currently focused window to the next workspace in the
--   given direction that satisfies the given condition.
shiftTo :: Direction1D -> WSType -> X ()
shiftTo dir t = doTo dir t getSortByIndex (windows . shift)

-- | Using the given sort, find the next workspace in the given
-- direction of the given type, and perform the given action on it.
doTo :: Direction1D -> WSType -> X WorkspaceSort -> (WorkspaceId -> X ()) -> X ()
doTo dir t srt act = findWorkspace srt dir t 1 >>= act

-- | Given a function @s@ to sort workspaces, a direction @dir@, a
--   predicate @p@ on workspaces, and an integer @n@, find the tag of
--   the workspace which is @n@ away from the current workspace in
--   direction @dir@ (wrapping around if necessary), among those
--   workspaces, sorted by @s@, which satisfy @p@.
--
--   For some useful workspace sorting functions, see
--   "XMonad.Util.WorkspaceCompare".
--
--   For ideas of what to do with a workspace tag once obtained, note
--   that 'moveTo' and 'shiftTo' are implemented by applying @(>>=
--   (windows . greedyView))@ and @(>>= (windows . shift))@, respectively,
--   to the output of 'findWorkspace'.
findWorkspace :: X WorkspaceSort -> Direction1D -> WSType -> Int -> X WorkspaceId
findWorkspace s dir t n = findWorkspaceGen s (wsTypeToPred t) (maybeNegate dir n)
  where
    maybeNegate Next d = d
    maybeNegate Prev d = -d

findWorkspaceGen :: X WorkspaceSort -> X (WindowSpace -> Bool) -> Int -> X WorkspaceId
findWorkspaceGen _ _ 0 = gets (currentTag . windowset)
findWorkspaceGen sortX wsPredX d = do
    wsPred <- wsPredX
    sort   <- sortX
    ws     <- gets windowset
    let cur     = workspace (current ws)
        sorted  = sort (workspaces ws)
        pivoted = let (a,b) = span ((/= tag cur) . tag) sorted in b ++ a
        ws'     = filter wsPred pivoted
        mCurIx  = findWsIndex cur ws'
        d'      = if d > 0 then d - 1 else d
        next    = if null ws'
                      then cur
                      else case mCurIx of
                            Nothing -> ws' !! (d' `mod` length ws')
                            Just ix -> ws' !! ((ix + d) `mod` length ws')
    return $ tag next

findWsIndex :: WindowSpace -> [WindowSpace] -> Maybe Int
findWsIndex ws = findIndex ((== tag ws) . tag)

