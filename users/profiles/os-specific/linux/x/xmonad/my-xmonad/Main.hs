{-# LANGUAGE DeriveGeneric #-}

module Main where

import Rice

import XMonad hiding (terminal)
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks

import XMonad.Layout.Dwindle
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.EntryHelper
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

import qualified DBus.Client as DC
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import GHC.Generics
import System.Exit
import qualified XMonad.DBus as D

-- layouts {{{
myLayout rice =
  avoidStruts $
    toggleLayouts (noBorders Full) $
      tall ||| tabs
 where
  gaps = spacingWithEdge 5
  theme =
    def
      { activeColor = base0F rice
      , activeBorderColor = base0F rice
      , activeTextColor = base00 rice
      , inactiveColor = base02 rice
      , inactiveBorderColor = base02 rice
      , inactiveTextColor = base03 rice
      , urgentColor = base08 rice
      , urgentBorderColor = base08 rice
      , urgentTextColor = base00 rice
      , fontName = "xft:" ++ uiFont rice
      }
  tall = gaps $ ResizableTall 1 (3 / 100) 0.5 []
  tabs = noBorders $ tabbedBottom shrinkText theme

-- }}}

-- keybindings {{{
myKeys :: Rice -> [(String, X ())]
myKeys rice =
  [ ("M-C-r", io sendRestart)
  , ("M-C-q", io exitSuccess)
  , ("M-<Return>", spawn $ terminal rice)
  , ("M-w", kill)
  , -- layout manipulation
    ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-S-h", sendMessage $ IncMasterN 1)
  , ("M-S-l", sendMessage $ IncMasterN (-1))
  , ("M-<Tab>", sendMessage NextLayout)
  , ("M-t", withFocused $ windows . W.sink)
  , ("M-f", sendMessage (Toggle "Full") <> sendMessage ToggleStruts)
  ]
    -- switch to workspace
    ++ [ ("M-" ++ show x, windows $ W.greedyView (show x))
       | x <- [1 .. 9]
       ]
    -- move to workspace
    ++ [ ("M-S-" ++ show x, windows $ W.shift (show x))
       | x <- [1 .. 9]
       ]

-- }}}

-- log hook {{{
data LogWorkspace = LogWorkspace
  { current :: Bool
  , id :: WorkspaceId
  }
  deriving (Generic)
instance ToJSON LogWorkspace

data LogWindow = LogWindow
  { focused :: Bool
  , title :: String
  }
  deriving (Generic)
instance ToJSON LogWindow

myLogHook :: DC.Client -> PP
myLogHook dbus =
  def
    { ppOutput = D.send dbus . wrap "{" "}"
    , ppWsSep = ","
    , ppSep = ","
    , ppOrder = \(ws : l : _ : extras) ->
        [ wrap "\"workspaces\":[" "]" ws
        , wrap "\"layout\":\"" "\"" l
        ]
          ++ extras
    , ppHidden = enc . LogWorkspace False
    , ppCurrent = enc . LogWorkspace True
    , ppExtras = [wrapL "\"title\":\"" "\"" logTitle .| logConst "\"title\":\"\""]
    }
 where
  enc :: (ToJSON t) => t -> String
  enc = BLU.toString . encode

-- }}}

-- startup hook {{{
myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr

-- }}}

main :: IO ()
main = withHelper $ do
  dbus <- D.connect
  D.requestAccess dbus

  rice <- getRice
  xmonad
    . fullscreenSupport
    . ewmhFullscreen
    . ewmh
    . docks
    $ def
      { modMask = mod1Mask
      , layoutHook = myLayout rice
      , startupHook = myStartupHook
      , borderWidth = 2
      , normalBorderColor = base00 rice
      , focusedBorderColor = base0F rice
      , keys = (`mkKeymap` myKeys rice)
      , logHook = dynamicLogWithPP $ myLogHook dbus
      }
