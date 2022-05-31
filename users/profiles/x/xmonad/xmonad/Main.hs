module Main where

import Data.Tree
import Data.Word
import Text.Read (read)

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.TreeSelect

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.Dwindle
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing (spacingWithEdge)
import XMonad.Layout.Tabbed

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.Cursor
import XMonad.Util.EZConfig (additionalKeysP, mkKeymap)

import Rice

-- layouts {{{
myLayout = avoidStruts (tall ||| Full)
  where
    tall = renamed [Replace "\xF0BCC"] $ gaps $ ResizableTall 1 (3 / 100) 0.5 []
    tabbed = renamed [Replace "\xF1888"] simpleTabbed

    gaps = spacingWithEdge 5

-- }}}

-- keybindings {{{
myKeys :: Rice -> [(String, X ())]
myKeys rice =
  [ ("M-C-r", spawn "my-xmonad --restart")
  , ("M-C-q", io exitSuccess)
  , ("M-<Return>", spawn "kitty")
  , ("M-w", kill)
  , -- layout manipulation
    ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-S-h", sendMessage $ IncMasterN 1)
  , ("M-s-l", sendMessage $ IncMasterN (-1))
  , ("M-<Tab>", sendMessage NextLayout)
  , ("M-t", withFocused $ windows . W.sink)
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

-- bar pretty-printer {{{
myXmobarPP :: Rice -> PP
myXmobarPP rice =
  def
    { ppSep = xmobarFont 1 " "
    , ppWsSep = ""
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = tag base0F base00
    , ppHidden = tag base02 base03
    , ppUrgent = tag base02 base08
    , ppOrder = \(ws : l : t : ex) -> [l, ws, t]
    , ppLayout = xmobarFont 2 . xmobarColor (base03 rice) ""
    }
  where
    tag bg fg =
      xmobarFont 1
        . xmobarColor (fg rice) (bg rice)
        . xmobarBorder "VBoth" (bg rice) 5
        . pad

-- }}}

main :: IO ()
main = do
  rice <- getRice
  xmonad
    . ewmhFullscreen
    . ewmh
    . docks
    . withEasySB (statusBarProp "my-xmobar" (pure $ myXmobarPP rice)) defToggleStrutsKey
    $ def
      { modMask = mod4Mask
      , layoutHook = myLayout
      , startupHook = setDefaultCursor xC_left_ptr
      , borderWidth = 2
      , normalBorderColor = base00 rice
      , focusedBorderColor = base0F rice
      , keys = (`mkKeymap` myKeys rice)
      }
