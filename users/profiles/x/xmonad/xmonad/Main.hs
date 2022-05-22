module Main where

import XMonad

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

import XMonad.Util.Cursor
import XMonad.Util.EZConfig (additionalKeysP)

import Rice

-- layouts {{{
myLayout = avoidStruts (tall ||| Full)
  where
    tall = renamed [Replace "\xF0BCC"] $ gaps $ ResizableTall 1 (3 / 100) 0.5 []
    tabbed = renamed [Replace "\xF1888"] simpleTabbed

    gaps = spacingWithEdge 5

-- }}}

-- keybindings {{{
myKeys :: [(String, X ())]
myKeys =
  [ ("M-<Return>", spawn "kitty")
  , ("M-C-r", spawn "my-xmonad --restart")
  , ("M-C-q", io exitSuccess)
  , ("M-w", kill)
  , ("M-<Tab>", sendMessage NextLayout)
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
  -- mySB <- statusBarPipe "my-xmobar" $ pure (myXmobarPP rice)
  xmonad
    . ewmhFullscreen
    . ewmh
    . docks
    -- . withEasySB mySB defToggleStrutsKey
    . withEasySB (statusBarProp "my-xmobar" (pure $ myXmobarPP rice)) defToggleStrutsKey
    $ def
      { modMask = mod4Mask
      , layoutHook = myLayout
      , startupHook = setDefaultCursor xC_left_ptr
      , borderWidth = 2
      , normalBorderColor = base00 rice
      , focusedBorderColor = base0F rice
      }
      `additionalKeysP` myKeys
